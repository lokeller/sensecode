/* Base $Id: SenseCodeForwardingEngineP.nc,v 1.23 2009/08/15 18:11:30 gnawali Exp $ */
/*
 * Copyright (c) 2011 EPFL, Lausanne
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
 * - Neither the name of the EPFL nor the names of
 *   its contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL EPFL
 *  OR ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * Derived from code: 
 * Copyright (c) 2008-9 Stanford University.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
 * - Neither the name of the Stanford University nor the names of
 *   its contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL STANFORD
 * UNIVERSITY OR ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */


#include <SenseCodeForwardingEngine.h>
#include <CtpDebugMsg.h>
#include <finite_field.h>
   
generic module SenseCodeForwardingEngineP() {
  provides {
    interface Init;
    interface StdControl;
    interface Send[uint8_t client];
    interface Receive[collection_id_t id];
    interface Receive as Snoop[collection_id_t id];
    interface Intercept[collection_id_t id];
    interface Packet;
    interface CollectionPacket;
    interface SenseCodePacket;
    interface CtpCongestion;
    interface SenseCodeControl;
  }
  uses {
    // These five interfaces are used in the forwarding path
    //   SubSend is for sending packets
    //   PacketAcknowledgements is for enabling layer 2 acknowledgments
    //   RetxmitTimer is for timing packet sends for improved performance
    //   LinkEstimator is for providing the ack bit to a link estimator
    interface AMSend as SubSend;
    interface PacketAcknowledgements;
    interface Timer<TMilli> as RetxmitTimer;
    interface LinkEstimator; 
    interface UnicastNameFreeRouting;
    interface Packet as SubPacket;

    // These four data structures are used to manage packets to forward.
    // SendQueue and QEntryPool are the forwarding queue.
    // MessagePool is the buffer pool for messages to forward.
    // SentCache is for suppressing duplicate packet transmissions.
    interface Queue<fe_queue_entry_t*> as SendQueue;
    interface Pool<fe_queue_entry_t> as QEntryPool;
    interface Pool<message_t> as MessagePool;
    interface Pool<message_t> as DecodingMessagePool;
    interface Cache<message_t*> as SentCache;
    
    interface Receive as SubReceive;
    interface Receive as SubSnoop;
    interface CtpInfo;
    interface RootControl;
    interface CollectionId[uint8_t client];
    interface AMPacket;
    interface Leds;
    interface Random;

    // This implementation has extensive debugging instrumentation.
    // Wiring up the CollectionDebug interface provides information
    // on important events, such as transmissions, receptions,
    // and cache checks. The TinyOS release includes scripts for
    // parsing these messages.
    interface CollectionDebug;

    
    // The ForwardingEngine monitors whether the underlying
    // radio is on or not in order to start/stop forwarding
    // as appropriate.
    interface SplitControl as RadioControl;
  }
}
implementation {
  /* Helper functions to start the given timer with a random number
   * masked by the given mask and added to the given offset.
   */
  static void startRetxmitTimer(uint16_t mask, uint16_t offset);
  void clearState(uint8_t state);
  bool hasState(uint8_t state);
  void setState(uint8_t state);

  // CTP state variables.
  enum {
    QUEUE_CONGESTED  = 0x1, // Need to set C bit?
    ROUTING_ON       = 0x2, // Forwarding running?
    RADIO_ON         = 0x4, // Radio is on?
    ACK_PENDING      = 0x8, // Have an ACK pending?
    SENDING          = 0x10 // Am sending a packet?
  };

  // Start with all states false
  uint8_t forwardingState = 0; 
  
  /* Keep track of the last parent address we sent to, so that
     unacked packets to an old parent are not incorrectly attributed
     to a new parent. */
  am_addr_t lastParent;
  
  /* Network-level sequence number, so that receivers
   * can distinguish retransmissions from different packets. */
  uint8_t seqno;

  enum {
    CLIENT_COUNT = uniqueCount(UQ_CTP_CLIENT)
  };

  /* Each sending client has its own reserved queue entry.
     If the client has a packet pending, its queue entry is in the 
     queue, and its clientPtr is NULL. If the client is idle,
     its queue entry is pointed to by clientPtrs. */

  fe_queue_entry_t clientEntries[CLIENT_COUNT];
  fe_queue_entry_t* ONE_NOK clientPtrs[CLIENT_COUNT];

  /* The loopback message is for when a collection roots calls
     Send.send. Since Send passes a pointer but Receive allows
     buffer swaps, the forwarder copies the sent packet into 
     the loopbackMsgPtr and performs a buffer swap with it.
     See sendTask(). */
     
  message_t loopbackMsg;
  message_t* ONE_NOK loopbackMsgPtr;

  uint8_t redundancy = 0;
  bool coding = TRUE;
  bool systematic_coding = TRUE;
  uint16_t block_interval = 0;

  uint16_t dec_generation = 0;
  message_t *dec_messages[MAX_NODES];
  bool src_decoded[MAX_NODES];
  uint16_t dec_count = 0;
  message_t decodedMsg;
  message_t *decodedPtr;

  bool buffer_initialized = FALSE;
  uint16_t buffered_generation = 0;
  message_t buffered_messages[BUFFER_SIZE];

  uint32_t lost_queue = 0;
  uint32_t lost_retx = 0;
  uint32_t sent = 0;

  void setupGeneration(uint16_t generation, uint16_t payloadLen) {
	int i;

	dbg("CtpStats", "Generation %d Lost queue %d Lost retx %d Sent %d\n", buffered_generation, lost_queue, lost_retx, sent);	
	lost_queue = 0;
	lost_retx = 0;
	sent = 0;

	/* reset the buffered messages */
	for ( i= 0 ; i < BUFFER_SIZE ; i++) {
		memset(&(buffered_messages[i]), 0, sizeof(message_t));
		call SenseCodePacket.setGeneration(&(buffered_messages[i]), generation);
		call SenseCodePacket.setOption(&(buffered_messages[i]), CTP_OPT_CODED);
		call Packet.setPayloadLength(&(buffered_messages[i]), payloadLen);
	}

	/* update the generation */
	buffered_generation = generation;

	buffer_initialized = TRUE;
  }

  command error_t Init.init() {
    int i;
    for (i = 0; i < CLIENT_COUNT; i++) {
      clientPtrs[i] = clientEntries + i;
      dbg("Forwarder", "clientPtrs[%hhu] = %p\n", i, clientPtrs[i]);
    }

    for ( i= 0 ; i < MAX_CODING_VECTOR_LEN * 2 ; i++)
	src_decoded[i] = FALSE;

    decodedPtr = &decodedMsg;

    loopbackMsgPtr = &loopbackMsg;
    lastParent = call AMPacket.address();
    seqno = 0;
    return SUCCESS;
  }

  command error_t StdControl.start() {
    setState(ROUTING_ON);
    return SUCCESS;
  }

  command error_t StdControl.stop() {
    clearState(ROUTING_ON);
    return SUCCESS;
  }

  /* sendTask is where the first phase of all send logic
   * exists (the second phase is in SubSend.sendDone()). */
  task void sendTask();
  
  /* ForwardingEngine keeps track of whether the underlying
     radio is powered on. If not, it enqueues packets;
     when it turns on, it then starts sending packets. */ 
  event void RadioControl.startDone(error_t err) {
    if (err == SUCCESS) {
      setState(RADIO_ON);
      if (!call SendQueue.empty()) {
	dbg("FHangBug", "%s posted sendTask.\n", __FUNCTION__);
        post sendTask();
      }
    }
  }

  static void startRetxmitTimer(uint16_t window, uint16_t offset) {
    uint16_t r = call Random.rand16();
    r %= window;
    r += offset;
    call RetxmitTimer.startOneShot(r);
    dbg("Forwarder", "Rexmit timer will fire in %hu ms\n", r);
  }
  
  /* 
   * If the ForwardingEngine has stopped sending packets because
   * these has been no route, then as soon as one is found, start
   * sending packets.
   */ 
  event void UnicastNameFreeRouting.routeFound() {
    dbg("FHangBug", "%s posted sendTask.\n", __FUNCTION__);
    post sendTask();
  }

  event void UnicastNameFreeRouting.noRoute() {
    // Depend on the sendTask to take care of this case;
    // if there is no route the component will just resume
    // operation on the routeFound event
  }
  
  event void RadioControl.stopDone(error_t err) {
    if (err == SUCCESS) {
      clearState(RADIO_ON);
    }
  }

  ctp_data_header_t* getHeader(message_t* m) {
    return (ctp_data_header_t*)call SubPacket.getPayload(m, sizeof(ctp_data_header_t));
  }


#ifdef TOSSIM
  void dumpMessage(char *prefix, message_t *msg) {

	char cv[500];
	char p[500];
	if (call SenseCodePacket.option(msg, CTP_OPT_CODED) ) {
		dbg("SenseCodePacket", "%s : Generation %d (%d : seqno %d) %s ( %s)\n", 
			prefix, 
			call SenseCodePacket.getGeneration(msg),
			call SenseCodePacket.getOrigin(msg),
			call CollectionPacket.getSequenceNumber(msg),
			ff_dump_vector(cv, call SenseCodePacket.getCodingVectorData(msg), MAX_CODING_VECTOR_LEN * 2),
			ff_dump_vector(p, call Packet.getPayload(msg, call Packet.payloadLength(msg)), call Packet.payloadLength(msg) * 2));
	} else {
		dbg("SenseCodePacket", "%s : Generation %d (%d : seqno %d) ( %s)\n", 
			prefix, 
			call SenseCodePacket.getGeneration(msg),
			call SenseCodePacket.getOrigin(msg),
			call CollectionPacket.getSequenceNumber(msg),
			ff_dump_vector(p, call Packet.getPayload(msg, call Packet.payloadLength(msg)), call Packet.payloadLength(msg) * 2));
	}
  }
#endif

  void combinePacket(message_t *dest, message_t *src, uint8_t coeff) {

	uint16_t packetLen = call Packet.payloadLength(src);

	/* do not combine if the destination has no coding vector attached */
	if (!call SenseCodePacket.option(dest, CTP_OPT_CODED)) {
		return;
	}

	/* if the packet is in the same generation and has a lenght equal to
	 the incoming packing, merge */
	if ( getHeader(dest)->generation == getHeader(src)->generation &&
		call Packet.payloadLength(dest) == packetLen ) {
		uint8_t * dest_v = call SenseCodePacket.getCodingVectorData(dest);

		/* if the source packet is coded sum the coding vectors */
		if ( call SenseCodePacket.option(src, CTP_OPT_CODED)) {
			uint8_t * src_v = call SenseCodePacket.getCodingVectorData(src);
			ff_mul_sum(dest_v, src_v, coeff, MAX_CODING_VECTOR_LEN * 2);

		/* otherwise add to the proper coordinate the coefficient */
		} else {
			uint16_t id = call SenseCodePacket.getOrigin(src);
			uint8_t cc = ff_get_c(dest_v, id);

			ff_set_c(dest_v, id, ff_sum_table[cc][coeff]);
				
		}

		/* sum the payloads */
		ff_mul_sum(call Packet.getPayload(dest, packetLen), 
				call Packet.getPayload(src, packetLen),
				coeff,
				packetLen * 2);
	}
  }

  void mergeOnQueue(message_t *msg) {

	int i;
	uint16_t generation = getHeader(msg)->generation;
	
	/* if this packet of an old generation, ignore it */
	if ( generation < buffered_generation)
		return;

	/* if this packet belongs to a new generation, or we didn't initialize
		the buffer yet, clean the buffer and update the generation */
	if ( generation > buffered_generation || !buffer_initialized) {
		setupGeneration(generation, call Packet.payloadLength(msg));
	}

	/* merge with the packets on the queue */
	for ( i = 0 ; i < call SendQueue.size() ; i++ ) {
		fe_queue_entry_t* entry = call SendQueue.element(i);		

		uint8_t r = call Random.rand16() % FIELD_SIZE;
		combinePacket(entry->msg, msg, r);
	}

	/* merge with the packets in the buffer */
	for ( i = 0 ; i < BUFFER_SIZE ; i++ ) {
		uint8_t r = call Random.rand16() % FIELD_SIZE;
		combinePacket(&(buffered_messages[i]), msg, r);
	}
  }


  void rebuildQueueEntry(fe_queue_entry_t *ce) {

	int i;

	/* combine with the packets in the queue */
	for ( i = 0 ; i < call SendQueue.size() ; i++ ) {
		fe_queue_entry_t* entry = call SendQueue.element(i);		
		uint8_t r = call Random.rand16() % FIELD_SIZE;

		/* don't add the vector to itself */
		if (entry == ce) {
		 continue;
		}

		combinePacket(ce->msg, entry->msg, r);
	}
	
	/* combine with packets from the buffer */
	for ( i = 0 ; i < BUFFER_SIZE ; i++ ) {
		uint8_t r = call Random.rand16() % FIELD_SIZE;
		combinePacket(ce->msg, &(buffered_messages[i]), r);
	}
  }

  void addSimplified(message_t *msg) {
		
	int pivot, pos, i, j;
	uint8_t *v = call SenseCodePacket.getCodingVectorData(msg);

	pivot = ff_find_pivot(v, MAX_CODING_VECTOR_LEN * 2);

	pos = dec_count;
	for (i = 0 ; i < dec_count ; i++) {
		int lpivot;
		uint8_t *other = call SenseCodePacket.getCodingVectorData(dec_messages[i]);
		lpivot = ff_find_pivot(other, MAX_CODING_VECTOR_LEN * 2); 
		if ( pivot < lpivot) {
			pos = i;
			break;
		} else {
			uint8_t pk = ff_get_c(other , pivot);
			/* substract coding vector */
			ff_mul_sub(other, v, pk, MAX_CODING_VECTOR_LEN * 2);
			/* substract payload vector */	
			ff_mul_sub(call Packet.getPayload(dec_messages[i],
							call Packet.payloadLength(dec_messages[i])),
				   call Packet.getPayload(msg,
							call Packet.payloadLength(msg)),
				    pk, 
				   call Packet.payloadLength(dec_messages[i]) * 2);

			for ( j = i+1 ; j < dec_count ; j++) {
				uint8_t *yet_another = call SenseCodePacket.getCodingVectorData(dec_messages[j]);
				int k;
				k = ff_find_pivot(yet_another, MAX_CODING_VECTOR_LEN * 2);
				pk = ff_get_c(other, k);

				/* substract coding vector */
				ff_mul_sub(other, yet_another, pk, MAX_CODING_VECTOR_LEN * 2);
				
				/* substarct payload vector */
				ff_mul_sub(call Packet.getPayload(dec_messages[i],
								call Packet.payloadLength(dec_messages[i])),
					   call Packet.getPayload(dec_messages[j],
								call Packet.payloadLength(dec_messages[j])),
					    pk, 
					   call Packet.payloadLength(dec_messages[i]) * 2);
			}
		}
	}

	for ( i = dec_count ; i > pos ; i--) {
		dec_messages[i] = dec_messages[i-1];
	}
	dec_messages[i] = msg;
	dec_count++;

  }

  bool simplifyVector(message_t *msg) {
	int i, j;

	uint8_t *vector = call SenseCodePacket.getCodingVectorData(msg);
	
	for (i = 0 ; i < dec_count ; i++) {
		uint8_t *other = call SenseCodePacket.getCodingVectorData(dec_messages[i]);
		for ( j = 0 ; j < MAX_CODING_VECTOR_LEN * 2 ; j++) {
			uint8_t v,u;

			v = ff_get_c(other, j);
			u = ff_get_c(vector, j);
			if ( v > 0) {
				if ( u > 0) {
					/* substract coding vector */
					ff_mul_sub(vector, other, u, MAX_CODING_VECTOR_LEN * 2); 
					/* substarct payload vector */
					ff_mul_sub(call Packet.getPayload(msg,
									call Packet.payloadLength(msg)),
						   call Packet.getPayload(dec_messages[i],
									call Packet.payloadLength(dec_messages[i])),
						   u , 
						   call Packet.payloadLength(dec_messages[i]) * 2);
				}
				break;
			}

		}
	}

	if ( !ff_is_zero(vector, MAX_CODING_VECTOR_LEN * 2)) {
		uint8_t pivot_value = ff_get_c(vector, ff_find_pivot(vector, MAX_CODING_VECTOR_LEN * 2));
		if ( pivot_value != 1 ) {
			/* divide the coding vector */
			ff_div(vector, pivot_value, MAX_CODING_VECTOR_LEN * 2);
			/* divide the payload vector */
			ff_div(call Packet.getPayload(msg, call Packet.payloadLength(msg)), 
				pivot_value, 
				call Packet.payloadLength(msg) * 2);
		}

		return TRUE;
	} else {
		return FALSE;
	}
	
  }

  void codePacket(message_t *msg, uint16_t len) {
	
	uint8_t* cv;

	call SenseCodePacket.setOption(msg, CTP_OPT_CODED);

        /* set the packet length, if coding has been enabled the appropriate
        amount of bytes to store the coding vector will also be allocated */
        call Packet.setPayloadLength(msg, len);
	
	cv = call SenseCodePacket.getCodingVectorData(msg);
	
	/* set coding vector to zero */
	memset(cv, 0, MAX_CODING_VECTOR_LEN);

	ff_set_c(cv, call SenseCodePacket.getOrigin(msg), 1);
	
	mergeOnQueue(msg);

  }

  uint32_t received_overhearing = 0;
  uint32_t received_directed = 0;
  uint32_t received_old = 0;

  message_t *putInDecodingBuffer(message_t *msg) {
	int i;	
	uint16_t gen = getHeader(msg)->generation;
	collection_id_t collectid = getHeader(msg)->type;


#ifdef TOSSIM
	dumpMessage("Received message at sink", msg);
#endif

	/* the packet is from an old generation, discard */
	if (gen < dec_generation) {
		dbg("ApplicationDetails", "Received from previous generation\n");
		received_old++;
		return msg;
	}

	/* the packet is from a new generation, flush decoding messages in buffer */
	if (gen > dec_generation) {

		dbg("CtpStats", "Generation %d Received overhearing %d Received directed %d Received old: %d\n", dec_generation, received_overhearing, received_directed, received_old);
		received_directed = 0;
		received_overhearing = 0;
		received_old = 0;

		for (i = 0; i < dec_count ; i++) {
			call DecodingMessagePool.put(dec_messages[i]);
		}

    		for ( i= 0 ; i < MAX_CODING_VECTOR_LEN * 2 ; i++)
			src_decoded[i] = FALSE;

		dec_count = 0;
		dec_generation = gen;
		dbg("ApplicationDetails", "Received from new generation\n");
	}

	/* the packet is not coded , code it */
	if ( !call SenseCodePacket.option(msg, CTP_OPT_CODED)) {
		codePacket(msg, call Packet.payloadLength(msg));
	}

	/* simplify the incoming packet */
	if ( simplifyVector(msg)) {
		message_t *ret = call DecodingMessagePool.get();

		/* cannot get a message from the pool, we cannot store the incoming one */
		if (ret == NULL) {
			dbg("Application", "Message pool empty\n");
			return msg;
		}

		/* the packet after simplification is non zero, add it to the decoding buffer */
		addSimplified(msg);
		
		/* look for decoded packets in the buffer and signal them */
		for ( i = 0 ; i < dec_count ; i++) {
			uint16_t c = ff_is_canonical(call SenseCodePacket.getCodingVectorData(dec_messages[i]), MAX_CODING_VECTOR_LEN * 2);
			
			/* if the packet has been decoded and has not been signaled yet */
			if (c != 0xFFFF && !src_decoded[c]) {

				/* mark the source as decoded */
				src_decoded[c] = TRUE;

				/* set the origin to the correct origin (after decoding) */
				getHeader(dec_messages[i])->origin = c;
				
				/* create a copy that will be sent to upper layer */
				memcpy(decodedPtr, dec_messages[i], sizeof(message_t));				
				/* signal the packet arrival, the upper layer may do a buffer swap */
      				decodedPtr = signal Receive.receive[collectid](decodedPtr, 
					       call Packet.getPayload(decodedPtr, call Packet.payloadLength(decodedPtr)), 
					       call Packet.payloadLength(decodedPtr));
			}		
		}

		return ret;
	} else {
		dbg("ApplicationDetails", "Already received, discarding\n");
		return msg;
	}
  }

 
  /*
   * The send call from a client. Return EBUSY if the client is busy
   * (clientPtrs is NULL), otherwise configure its queue entry
   * and put it in the send queue. If the ForwardingEngine is not
   * already sending packets (the RetxmitTimer isn't running), post
   * sendTask. It could be that the engine is running and sendTask
   * has already been posted, but the post-once semantics make this
   * not matter. What's important is that you don't post sendTask
   * if the retransmit timer is running; this would circumvent the
   * timer and send a packet before it fires.
   */ 
  command error_t Send.send[uint8_t client](message_t* msg, uint8_t len) {
    ctp_data_header_t* hdr;
    fe_queue_entry_t *qe;

    if (TOS_NODE_ID >= MAX_NODES) return FAIL;

    dbg("Forwarder", "%s: sending packet from client %hhu: %x, len %hhu\n", __FUNCTION__, client, msg, len);
    if (!hasState(ROUTING_ON)) {return EOFF;}
    if (len > call Send.maxPayloadLength[client]()) {return ESIZE;}

    /* prepare the packet as if it doens't need to be coded, we will
       setup coding headers later */
    call SenseCodePacket.clearOption(msg, CTP_OPT_CODED);
    call Packet.setPayloadLength(msg, len);


    hdr = getHeader(msg);
    hdr->origin = TOS_NODE_ID;
    hdr->originSeqNo = seqno ;
    seqno += redundancy + 1;
    hdr->type = call CollectionId.fetch[client]();
    hdr->thl = 0;

    /* set the coding header for the packet if systematic coding is
	not enabled*/
    if (coding && !systematic_coding) {
  	codePacket(msg, len);
    }

    if (clientPtrs[client] == NULL) {
      dbg("Forwarder", "%s: send failed as client is busy.\n", __FUNCTION__);
      return EBUSY;
    }

    qe = clientPtrs[client];
    qe->msg = msg;
    qe->client = client;
    qe->retries = MAX_RETRIES;
    qe->redundancy = redundancy;
    dbg("Forwarder", "%s: queue entry for %hhu is %hhu deep\n", __FUNCTION__, client, call SendQueue.size());
    if (call SendQueue.enqueue(qe) == SUCCESS) {
      if (hasState(RADIO_ON) && !hasState(SENDING)) {
	dbg("FHangBug", "%s posted sendTask.\n", __FUNCTION__);
        post sendTask();
      }
      clientPtrs[client] = NULL;
      return SUCCESS;
    }
    else {
      dbg("Forwarder", 
          "%s: send failed as packet could not be enqueued.\n", 
          __FUNCTION__);
      
      // send a debug message to the uart
      call CollectionDebug.logEvent(NET_C_FE_SEND_QUEUE_FULL);

      // Return the pool entry, as it's not for me...
      return FAIL;
    }
  }

  command error_t Send.cancel[uint8_t client](message_t* msg) {
    // cancel not implemented. will require being able
    // to pull entries out of the queue.
    return FAIL;
  }

  command uint8_t Send.maxPayloadLength[uint8_t client]() {
    return call Packet.maxPayloadLength();
  }

  command void* Send.getPayload[uint8_t client](message_t* msg, uint8_t len) {
    return call Packet.getPayload(msg, len);
  }

  /*
   * These is where all of the send logic is. When the ForwardingEngine
   * wants to send a packet, it posts this task. The send logic is
   * independent of whether it is a forwarded packet or a packet from
   * a send clientL the two cases differ in how memory is managed in
   * sendDone.
   *
   * The task first checks that there is a packet to send and that
   * there is a valid route. It then marshals the relevant arguments
   * and prepares the packet for sending. If the node is a collection
   * root, it signals Receive with the loopback message. Otherwise,
   * it sets the packet to be acknowledged and sends it. It does not
   * remove the packet from the send queue: while sending, the 
   * packet being sent is at the head of the queue; a packet is dequeued
   * in the sendDone handler, either due to retransmission failure
   * or to a successful send.
   */

  task void sendTask() {
    uint16_t gradient;
    dbg("Forwarder", "%s: Trying to send a packet. Queue size is %hhu.\n", __FUNCTION__, call SendQueue.size());
    if (hasState(SENDING) || call SendQueue.empty()) {
      call CollectionDebug.logEvent(NET_C_FE_SENDQUEUE_EMPTY);
      return;
    }
    else if ((!call RootControl.isRoot() && 
	      !call UnicastNameFreeRouting.hasRoute()) ||
	     (call CtpInfo.getEtx(&gradient) != SUCCESS)) {
      /* This code path is for when we don't have a valid next
       * hop. We set a retry timer.
       *
       * Technically, this timer isn't necessary, as if a route
       * is found we'll get an event. But just in case such an event
       * is lost (e.g., a bug in the routing engine), we retry.
       * Otherwise the forwarder might hang indefinitely. As this test
       * doesn't require radio activity, the energy cost is minimal. */
      dbg("Forwarder", "%s: no route, don't send, try again in %i.\n", __FUNCTION__, NO_ROUTE_RETRY);
      call RetxmitTimer.startOneShot(NO_ROUTE_RETRY);
      call CollectionDebug.logEvent(NET_C_FE_NO_ROUTE);
      return;
    }
    else {
      /* We can send a packet.
	 First check if it's a duplicate;
	 if not, try to send/forward. */
      error_t subsendResult;
      fe_queue_entry_t* qe = call SendQueue.head();
      uint8_t payloadLen = call SubPacket.payloadLength(qe->msg);
      am_addr_t dest = call UnicastNameFreeRouting.nextHop();

      if (call SentCache.lookup(qe->msg)) {
	/* This packet is a duplicate, so suppress it: free memory and
	 * send next packet.  Duplicates are only possible for
	 * forwarded packets, so we can circumvent the client or
	 * forwarded branch for freeing the buffer. */
        call CollectionDebug.logEvent(NET_C_FE_DUPLICATE_CACHE_AT_SEND);
        call SendQueue.dequeue();
	if (call MessagePool.put(qe->msg) != SUCCESS) 
	  call CollectionDebug.logEvent(NET_C_FE_PUT_MSGPOOL_ERR); 
	if (call QEntryPool.put(qe) != SUCCESS) 
	  call CollectionDebug.logEvent(NET_C_FE_PUT_QEPOOL_ERR); 
	  
        post sendTask();
        return;
      }
      
      // Not a duplicate: we've decided we're going to send.
      dbg("Forwarder", "Sending queue entry %p\n", qe);

      if (call RootControl.isRoot()) {
	/* Code path for roots: copy the packet and signal receive. */
	uint8_t* payload;
	uint8_t payloadLength;

        memcpy(loopbackMsgPtr, qe->msg, sizeof(message_t));

	payload = call Packet.getPayload(loopbackMsgPtr, call Packet.payloadLength(loopbackMsgPtr));
	payloadLength =  call Packet.payloadLength(loopbackMsgPtr);
        dbg("Forwarder", "%s: I'm a root, so loopback and signal receive.\n", __FUNCTION__);
	received_directed++;
	loopbackMsgPtr = putInDecodingBuffer(loopbackMsgPtr);
        signal SubSend.sendDone(qe->msg, SUCCESS);
      }
      else {
	/* The basic forwarding/sending case. */
	call SenseCodePacket.setEtx(qe->msg, gradient);
	call SenseCodePacket.clearOption(qe->msg, CTP_OPT_ECN | CTP_OPT_PULL);
	if (call PacketAcknowledgements.requestAck(qe->msg) == SUCCESS) {
	  setState(ACK_PENDING);
	}
	if (hasState(QUEUE_CONGESTED)) {
	  call SenseCodePacket.setOption(qe->msg, CTP_OPT_ECN); 
	  clearState(QUEUE_CONGESTED);
	}
	
	{
	      uint16_t etx, linketx; 
	      linketx = call LinkEstimator.getLinkQuality(dest);
		
	      call CtpInfo.getEtx(&etx);

	      dbg("ForwarderSend","Sending packet (origin: %d seqno: %d retx: %d path etx: %d link etx: %d) to %d\n", getHeader(qe->msg)->origin, getHeader(qe->msg)->originSeqNo, qe->retries, etx , linketx, dest);
     }

	rebuildQueueEntry(qe);

	sent++;
	
	subsendResult = call SubSend.send(dest, qe->msg, payloadLen);
	if (subsendResult == SUCCESS) {
	  // Successfully submitted to the data-link layer.
	  setState(SENDING);
	  dbg("Forwarder", "%s: subsend succeeded with %p.\n", __FUNCTION__, qe->msg);
	  return;
	}
	// The packet is too big: truncate it and retry.
	else if (subsendResult == ESIZE) {
	  dbg("Forwarder", "%s: subsend failed from ESIZE: truncate packet.\n", __FUNCTION__);
	  call Packet.setPayloadLength(qe->msg, call Packet.maxPayloadLength());
	  post sendTask();
	  call CollectionDebug.logEvent(NET_C_FE_SUBSEND_SIZE);
	}
	else {
	  dbg("Forwarder", "%s: subsend failed from %i\n", __FUNCTION__, (int)subsendResult);
	}
      }
    }
  }


  /*
   * The second phase of a send operation; based on whether the transmission was
   * successful, the ForwardingEngine either stops sending or starts the
   * RetxmitTimer with an interval based on what has occured. If the send was
   * successful or the maximum number of retransmissions has been reached, then
   * the ForwardingEngine dequeues the current packet. If the packet is from a
   * client it signals Send.sendDone(); if it is a forwarded packet it returns
   * the packet and queue entry to their respective pools.
   * 
   */

  void packetComplete(fe_queue_entry_t* qe, message_t* msg, bool success) {
    // Four cases:
    // Local packet: success or failure
    // Forwarded packet: success or failure
    if (qe->client < CLIENT_COUNT) { 
      clientPtrs[qe->client] = qe;
      signal Send.sendDone[qe->client](msg, SUCCESS);
      if (success) {
	dbg("CtpForwarder", "%s: packet %hu.%hhu for client %hhu acknowledged.\n", __FUNCTION__, call CollectionPacket.getOrigin(msg), call CollectionPacket.getSequenceNumber(msg), qe->client);
	call CollectionDebug.logEventMsg(NET_C_FE_SENT_MSG, 
					 call CollectionPacket.getSequenceNumber(msg), 
					 call CollectionPacket.getOrigin(msg), 
                                         call AMPacket.destination(msg));
      } else {
	dbg("CtpForwarder", "%s: packet %hu.%hhu for client %hhu dropped.\n", __FUNCTION__, call CollectionPacket.getOrigin(msg), call CollectionPacket.getSequenceNumber(msg), qe->client);
	call CollectionDebug.logEventMsg(NET_C_FE_SENDDONE_FAIL_ACK_SEND, 
					 call CollectionPacket.getSequenceNumber(msg), 
					 call CollectionPacket.getOrigin(msg), 
					 call AMPacket.destination(msg));
      }
    }
    else { 
      if (success) {
	call SentCache.insert(qe->msg);
	dbg("CtpForwarder", "%s: forwarded packet %hu.%hhu acknowledged: insert in transmit queue.\n", __FUNCTION__, call CollectionPacket.getOrigin(msg), call CollectionPacket.getSequenceNumber(msg));
	call CollectionDebug.logEventMsg(NET_C_FE_FWD_MSG, 
					 call CollectionPacket.getSequenceNumber(msg), 
					 call CollectionPacket.getOrigin(msg), 
                                         call AMPacket.destination(msg));
      }
      else {
	dbg("CtpForwarder", "%s: forwarded packet %hu.%hhu dropped.\n", __FUNCTION__, call CollectionPacket.getOrigin(msg), call CollectionPacket.getSequenceNumber(msg));
	call CollectionDebug.logEventMsg(NET_C_FE_SENDDONE_FAIL_ACK_FWD, 
					 call CollectionPacket.getSequenceNumber(msg), 
					 call CollectionPacket.getOrigin(msg), 
					 call AMPacket.destination(msg));
      }
      if (call MessagePool.put(qe->msg) != SUCCESS)
	call CollectionDebug.logEvent(NET_C_FE_PUT_MSGPOOL_ERR);
      if (call QEntryPool.put(qe) != SUCCESS)
	call CollectionDebug.logEvent(NET_C_FE_PUT_QEPOOL_ERR);
    }
  }
  
  event void SubSend.sendDone(message_t* msg, error_t error) {
    fe_queue_entry_t *qe = call SendQueue.head();
    dbg("Forwarder", "%s to %hu and %hhu\n", __FUNCTION__, call AMPacket.destination(msg), error);

    if (error != SUCCESS) {
      /* The radio wasn't able to send the packet: retransmit it. */
      dbg("Forwarder", "%s: send failed\n", __FUNCTION__);
      call CollectionDebug.logEventMsg(NET_C_FE_SENDDONE_FAIL, 
				       call CollectionPacket.getSequenceNumber(msg), 
				       call CollectionPacket.getOrigin(msg), 
				       call AMPacket.destination(msg));
      startRetxmitTimer(SENDDONE_FAIL_WINDOW, SENDDONE_FAIL_OFFSET);
    }
    else if (hasState(ACK_PENDING) && !call PacketAcknowledgements.wasAcked(msg)) {
      /* No ack: if countdown is not 0, retransmit, else drop the packet. */
      call LinkEstimator.txNoAck(call AMPacket.destination(msg));
      call CtpInfo.recomputeRoutes();
      if (--qe->retries) { 
#if NORMAL_BACKOFF
	int backoff = 1;
#else
	int backoff = 1 << ( MAX_RETRIES - qe->retries < 8 ? MAX_RETRIES - qe->retries : 8 );
#endif
        dbg("Forwarder", "%s: not acked, retransmit\n", __FUNCTION__);
        call CollectionDebug.logEventMsg(NET_C_FE_SENDDONE_WAITACK, 
					 call CollectionPacket.getSequenceNumber(msg), 
					 call CollectionPacket.getOrigin(msg), 
                                         call AMPacket.destination(msg));
	rebuildQueueEntry(qe);
        startRetxmitTimer(SENDDONE_NOACK_WINDOW * backoff, SENDDONE_NOACK_OFFSET);
      } else {
	/* Hit max retransmit threshold: drop the packet. */
	call SendQueue.dequeue();
        clearState(SENDING);
        startRetxmitTimer(SENDDONE_OK_WINDOW, SENDDONE_OK_OFFSET);
	
	packetComplete(qe, msg, FALSE);
	lost_retx++;
	dbg("CtpLosses", "Packet lost due: too many retransmissions\n");
      }
    }
    else {
      /* Packet was acknowledged. Updated the link estimator,
	 free the buffer (pool or sendDone), start timer to
	 send next packet. */
      clearState(SENDING);
      call LinkEstimator.txAck(call AMPacket.destination(msg));
      
      if ( qe->client < CLIENT_COUNT && qe->redundancy > 0) {
	     dbg("Forwarder", "%s: our packet (seqno = %d ) for client %hhu, sending next copy\n", 
          				__FUNCTION__, getHeader(qe->msg)->originSeqNo, qe->client, qe);

	     getHeader(qe->msg)->originSeqNo++;
	     qe->redundancy--;
	     qe->retries = MAX_RETRIES;
		
	    /* set the coding header for the packet if coding is enabled
		and the packet is not yet coded (this happens with systematic
		coding, the first copy of the packet in that case is sent not
		coded) */
	    if (coding && !call SenseCodePacket.option(qe->msg, CTP_OPT_CODED)) {		
	  	codePacket(qe->msg, call Packet.payloadLength(qe->msg));
	    }
	    startRetxmitTimer(SENDDONE_OK_WINDOW, SENDDONE_OK_OFFSET+block_interval);
      } else {
	     call SendQueue.dequeue();
             packetComplete(qe, msg, TRUE);
      	     startRetxmitTimer(SENDDONE_OK_WINDOW, SENDDONE_OK_OFFSET);
      }	 
    }
  }

  /*
   * Function for preparing a packet for forwarding. Performs
   * a buffer swap from the message pool. If there are no free
   * message in the pool, it returns the passed message and does not
   * put it on the send queue.
   */
  message_t* ONE forward(message_t* ONE m) {
    if (call MessagePool.empty()) {
      dbg("Route", "%s cannot forward, message pool empty.\n", __FUNCTION__);
      dbg("Forwarder", "%s: Pool empty, dropping packet\n", __FUNCTION__);
      // send a debug message to the uart
      call CollectionDebug.logEvent(NET_C_FE_MSG_POOL_EMPTY);
      lost_queue++;
      dbg("CtpLosses", "Packet lost due: message pool empty\n");
    }
    else if (call QEntryPool.empty()) {
      dbg("CtpLosses", "Packet lost due: queue entry pool empty\n");
      dbg("Route", "%s cannot forward, queue entry pool empty.\n", 
          __FUNCTION__);
      dbg("Forwarder", "%s: Queue pool empty, dropping packet\n", __FUNCTION__);
      // send a debug message to the uart
      call CollectionDebug.logEvent(NET_C_FE_QENTRY_POOL_EMPTY);
      lost_queue++;
    }
    else {
      message_t* newMsg;
      fe_queue_entry_t *qe;
      uint16_t gradient;
      
      qe = call QEntryPool.get();
      if (qe == NULL) {
        call CollectionDebug.logEvent(NET_C_FE_GET_MSGPOOL_ERR);
        return m;
      }

      newMsg = call MessagePool.get();
      if (newMsg == NULL) {
        call CollectionDebug.logEvent(NET_C_FE_GET_QEPOOL_ERR);
        return m;
      }

      memset(newMsg, 0, sizeof(message_t));
      // memset(m->metadata, 0, sizeof(message_metadata_t));
      
      qe->msg = m;
      qe->client = 0xff;
      qe->retries = MAX_RETRIES;
      qe->redundancy = 0;

      
      if (call SendQueue.enqueue(qe) == SUCCESS) {
        dbg("Forwarder,Route", "%s forwarding packet %p with queue size %hhu\n", __FUNCTION__, m, call SendQueue.size());
        // Loop-detection code:
        if (call CtpInfo.getEtx(&gradient) == SUCCESS) {
          // We only check for loops if we know our own metric
          if (call SenseCodePacket.getEtx(m) <= gradient) {
            // If our etx metric is less than or equal to the etx value
	    // on the packet (etx of the previous hop node), then we believe
	    // we are in a loop.
	    // Trigger a route update and backoff.
            call CtpInfo.triggerImmediateRouteUpdate();
            startRetxmitTimer(LOOPY_WINDOW, LOOPY_OFFSET);
            call CollectionDebug.logEventMsg(NET_C_FE_LOOP_DETECTED,
					 call CollectionPacket.getSequenceNumber(m), 
					 call CollectionPacket.getOrigin(m), 
                                         call AMPacket.destination(m));
          }
        }

        if (!call RetxmitTimer.isRunning()) {
          // sendTask is only immediately posted if we don't detect a
          // loop.
	  dbg("FHangBug", "%s: posted sendTask.\n", __FUNCTION__);
          post sendTask();
        }
        
        // Successful function exit point:
        return newMsg;
      } else {
        // There was a problem enqueuing to the send queue.
        if (call MessagePool.put(newMsg) != SUCCESS)
          call CollectionDebug.logEvent(NET_C_FE_PUT_MSGPOOL_ERR);
        if (call QEntryPool.put(qe) != SUCCESS)
          call CollectionDebug.logEvent(NET_C_FE_PUT_QEPOOL_ERR);
      }
    }

    // NB: at this point, we have a resource acquistion problem.
    // Log the event, and drop the
    // packet on the floor.

    call CollectionDebug.logEvent(NET_C_FE_SEND_QUEUE_FULL);
    return m;
  }
 
  /*
   * Received a message to forward. Check whether it is a duplicate by
   * checking the packets currently in the queue as well as the 
   * send history cache (in case we recently forwarded this packet).
   * The cache is important as nodes immediately forward packets
   * but wait a period before retransmitting after an ack failure.
   * If this node is a root, signal receive.
   */ 
  event message_t* 
  SubReceive.receive(message_t* msg, void* payload, uint8_t len) {
    collection_id_t collectid;
    bool duplicate = FALSE;
    fe_queue_entry_t* qe;
    uint8_t i, thl;


    collectid = call SenseCodePacket.getType(msg);

    // Update the THL here, since it has lived another hop, and so
    // that the root sees the correct THL.
    thl = call SenseCodePacket.getThl(msg);
    thl++;
    call SenseCodePacket.setThl(msg, thl);

    call CollectionDebug.logEventMsg(NET_C_FE_RCV_MSG,
					 call CollectionPacket.getSequenceNumber(msg), 
					 call CollectionPacket.getOrigin(msg), 
				     thl--);
    if (len > call SubSend.maxPayloadLength()) {
      return msg;
    }

    mergeOnQueue(msg);

    //See if we remember having seen this packet
    //We look in the sent cache ...
    if (call SentCache.lookup(msg)) {
        call CollectionDebug.logEvent(NET_C_FE_DUPLICATE_CACHE);
        return msg;
    }
    //... and in the queue for duplicates
    if (call SendQueue.size() > 0) {
      for (i = call SendQueue.size(); --i;) {
	qe = call SendQueue.element(i);
	if (call SenseCodePacket.matchInstance(qe->msg, msg)) {
	  duplicate = TRUE;
	  break;
	}
      }
    }
     


    // If I'm the root, signal receive. 
    if (call RootControl.isRoot()) {
	received_directed++;
	return putInDecodingBuffer(msg);
    } else if (duplicate) {
        dbg("Forwarder", "%s: Packet already in queue, dropping packet\n", __FUNCTION__);
        call CollectionDebug.logEvent(NET_C_FE_DUPLICATE_QUEUE);
        return msg;
    }
    // I'm on the routing path and Intercept indicates that I
    // should not forward the packet.
    else if (!signal Intercept.forward[collectid](msg, 
						  call Packet.getPayload(msg, call Packet.payloadLength(msg)), 
						  call Packet.payloadLength(msg)))
      return msg;
    else {
      dbg("Route", "Forwarding packet from %hu.\n", getHeader(msg)->origin);
      return forward(msg);
    }
  }


  event message_t* 
  SubSnoop.receive(message_t* msg, void *payload, uint8_t len) {
    //am_addr_t parent = call UnicastNameFreeRouting.nextHop();
    am_addr_t proximalSrc = call AMPacket.source(msg);
    
    mergeOnQueue(msg);

    // Check for the pull bit (P) [TEP123] and act accordingly.  This
    // check is made for all packets, not just ones addressed to us.
    if (call SenseCodePacket.option(msg, CTP_OPT_PULL)) {
      call CtpInfo.triggerRouteUpdate();
    }

    /* root delivers all snooped messages, we cannot send it to snoop becaues
       putInDecodingBuffer destroys the incoming message */
    if (call RootControl.isRoot()) {
	received_overhearing++;
	return putInDecodingBuffer(msg);
    } else {
        call CtpInfo.setNeighborCongested(proximalSrc, call SenseCodePacket.option(msg, CTP_OPT_ECN));
        return signal Snoop.receive[call SenseCodePacket.getType(msg)] 
	      (msg, payload + sizeof(ctp_data_header_t), 
	       len - sizeof(ctp_data_header_t));
    }
  }
  
  event void RetxmitTimer.fired() {
    clearState(SENDING);
    dbg("FHangBug", "%s posted sendTask.\n", __FUNCTION__);
    post sendTask();
  }

  command bool CtpCongestion.isCongested() {
    return FALSE;
  }

  command void CtpCongestion.setClientCongested(bool congested) {
    // Do not respond to congestion.
  }
  
  /* signalled when this neighbor is evicted from the neighbor table */
  event void LinkEstimator.evicted(am_addr_t neighbor) {}

  
  // Packet ADT commands
  command void Packet.clear(message_t* msg) {
    call SubPacket.clear(msg);
  }

  command uint8_t Packet.payloadLength(message_t* msg) {
    return call SubPacket.payloadLength(msg) - sizeof(ctp_data_header_t) - (call SenseCodePacket.option(msg, CTP_OPT_CODED) ? MAX_CODING_VECTOR_LEN : 0);
  }

  command void Packet.setPayloadLength(message_t* msg, uint8_t len) {
    call SubPacket.setPayloadLength(msg, len + sizeof(ctp_data_header_t) + (call SenseCodePacket.option(msg, CTP_OPT_CODED) ? MAX_CODING_VECTOR_LEN : 0));
  }
  
  command uint8_t Packet.maxPayloadLength() {
    return call SubPacket.maxPayloadLength() - sizeof(ctp_data_header_t) - MAX_CODING_VECTOR_LEN;
  }

  command void* Packet.getPayload(message_t* msg, uint8_t len) {
    uint8_t* payload = call SubPacket.getPayload(msg, len + sizeof(ctp_data_header_t));
    if (payload != NULL) {
      payload += sizeof(ctp_data_header_t);
    }
    return payload;
  }

  command uint16_t SenseCodePacket.getGeneration(message_t* msg) {return getHeader(msg)->generation;}
  command void SenseCodePacket.setGeneration(message_t* msg, uint16_t generation) { getHeader(msg)->generation = generation;}

  // CollectionPacket ADT commands
  command am_addr_t       CollectionPacket.getOrigin(message_t* msg) {return getHeader(msg)->origin;}
  command collection_id_t CollectionPacket.getType(message_t* msg) {return getHeader(msg)->type;}
  command uint8_t         CollectionPacket.getSequenceNumber(message_t* msg) {return getHeader(msg)->originSeqNo;}
  command void CollectionPacket.setOrigin(message_t* msg, am_addr_t addr) {getHeader(msg)->origin = addr;}
  command void CollectionPacket.setType(message_t* msg, collection_id_t id) {getHeader(msg)->type = id;}
  command void CollectionPacket.setSequenceNumber(message_t* msg, uint8_t _seqno) {getHeader(msg)->originSeqNo = _seqno;}

  // SenseCodePacket ADT commands
  command uint8_t       SenseCodePacket.getType(message_t* msg) {return getHeader(msg)->type;}
  command am_addr_t     SenseCodePacket.getOrigin(message_t* msg) {return getHeader(msg)->origin;}
  command uint16_t      SenseCodePacket.getEtx(message_t* msg) {return getHeader(msg)->etx;}
  command uint8_t       SenseCodePacket.getSequenceNumber(message_t* msg) {return getHeader(msg)->originSeqNo;}
  command uint8_t       SenseCodePacket.getThl(message_t* msg) {return getHeader(msg)->thl;}
  command void SenseCodePacket.setThl(message_t* msg, uint8_t thl) {getHeader(msg)->thl = thl;}
  command void SenseCodePacket.setOrigin(message_t* msg, am_addr_t addr) {getHeader(msg)->origin = addr;}
  command void SenseCodePacket.setType(message_t* msg, uint8_t id) {getHeader(msg)->type = id;}
  command void SenseCodePacket.setEtx(message_t* msg, uint16_t e) {getHeader(msg)->etx = e;}
  command void SenseCodePacket.setSequenceNumber(message_t* msg, uint8_t _seqno) {getHeader(msg)->originSeqNo = _seqno;}
  command bool SenseCodePacket.option(message_t* msg, ctp_options_t opt) {
    return ((getHeader(msg)->options & opt) == opt) ? TRUE : FALSE;
  }
  command void SenseCodePacket.setOption(message_t* msg, ctp_options_t opt) {
    getHeader(msg)->options |= opt;
  }
  command void SenseCodePacket.clearOption(message_t* msg, ctp_options_t opt) {
    getHeader(msg)->options &= ~opt;
  }


  command void SenseCodeControl.setRedundancy(uint8_t newVal) {
	redundancy = newVal;
  }

  command uint8_t SenseCodeControl.getRedundancy() {
	return redundancy;
  }

  command void SenseCodeControl.setCoding(bool enable) {
	coding = enable;
  }

  command bool SenseCodeControl.getCoding() {
	return coding;
  }

  command void SenseCodeControl.setSystematicCoding(bool enable) {
	systematic_coding = enable;
  }

  command bool SenseCodeControl.getSystematicCoding() {
	return systematic_coding;
  }

  command void SenseCodeControl.setBlockInterval(uint16_t blockInterval) {
        block_interval = blockInterval;
  }     

  command uint16_t SenseCodeControl.getBlockInterval() {
        return block_interval;
  }




  command void* SenseCodePacket.getCodingVectorData(message_t* msg) {
    uint8_t* payload = call SubPacket.getPayload(msg, call SubPacket.payloadLength(msg));
    if (payload != NULL) {
	payload += call SubPacket.payloadLength(msg) - MAX_CODING_VECTOR_LEN;
    }
    return payload;
  }

  // A CTP packet ID is based on the origin and the THL field, to
  // implement duplicate suppression as described in TEP 123.
  command bool SenseCodePacket.matchInstance(message_t* m1, message_t* m2) {
    return (call SenseCodePacket.getOrigin(m1) == call SenseCodePacket.getOrigin(m2) &&
	    call SenseCodePacket.getSequenceNumber(m1) == call SenseCodePacket.getSequenceNumber(m2) &&
	    call SenseCodePacket.getThl(m1) == call SenseCodePacket.getThl(m2) &&
	    call SenseCodePacket.getType(m1) == call SenseCodePacket.getType(m2));
  }

  command bool SenseCodePacket.matchPacket(message_t* m1, message_t* m2) {
    return (call SenseCodePacket.getOrigin(m1) == call SenseCodePacket.getOrigin(m2) &&
	    call SenseCodePacket.getSequenceNumber(m1) == call SenseCodePacket.getSequenceNumber(m2) &&
	    call SenseCodePacket.getType(m1) == call SenseCodePacket.getType(m2));
  }


  void clearState(uint8_t state) {
    forwardingState = forwardingState & ~state;
  }
  bool hasState(uint8_t state) {
    return forwardingState & state;
  }
  void setState(uint8_t state) {
    forwardingState = forwardingState | state;
  }
  
  /******** Defaults. **************/
   
  default event void
  Send.sendDone[uint8_t client](message_t *msg, error_t error) {
  }

  default event bool
  Intercept.forward[collection_id_t collectid](message_t* msg, void* payload, 
                                               uint8_t len) {
    return TRUE;
  }

  default event message_t *
  Receive.receive[collection_id_t collectid](message_t *msg, void *payload,
                                             uint8_t len) {
    return msg;
  }

  default event message_t *
  Snoop.receive[collection_id_t collectid](message_t *msg, void *payload,
                                           uint8_t len) {
    return msg;
  }

  default command collection_id_t CollectionId.fetch[uint8_t client]() {
    return 0;
  }
  
  /* Default implementations for CollectionDebug calls.
   * These allow CollectionDebug not to be wired to anything if debugging
   * is not desired. */
  
  default command error_t CollectionDebug.logEvent(uint8_t type) {
    return SUCCESS;
  }
  default command error_t CollectionDebug.logEventSimple(uint8_t type, uint16_t arg) {
    return SUCCESS;
  }
  default command error_t CollectionDebug.logEventDbg(uint8_t type, uint16_t arg1, uint16_t arg2, uint16_t arg3) {
    return SUCCESS;
  }
  default command error_t CollectionDebug.logEventMsg(uint8_t type, uint16_t msg, am_addr_t origin, am_addr_t node) {
    return SUCCESS;
  }
  default command error_t CollectionDebug.logEventRoute(uint8_t type, am_addr_t parent, uint8_t hopcount, uint16_t metric) {
    return SUCCESS;
  }
   
}

