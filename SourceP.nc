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
 */
module SourceP {
	uses interface Send;
	uses interface DisseminationValue<RequestMessage> as Request;
	uses interface SenseCodePacket;
	uses interface SenseCodeControl;
	uses interface Packet;
	uses interface Timer<TMilli> as BackoffTimer;
	uses interface Random;
	provides interface StdControl;
}

implementation {

	bool responseSendBusy = FALSE;

	typedef nx_struct {
		nx_uint32_t sentPackets;
	} stats_t;

	message_t message;
	stats_t stats;
	stats_t curr_stats;

	bool isRunning = FALSE;

	void sendPacket() {

		ResponseMessage *rm;
		const RequestMessage* newVal = call Request.get();

		call SenseCodeControl.setRedundancy(newVal->redundancy);
		call SenseCodeControl.setCoding(newVal->coding);
		call SenseCodeControl.setSystematicCoding(newVal->systematic);

		call SenseCodePacket.setGeneration(&message, newVal->id);

		call Packet.setPayloadLength(&message, sizeof(stats_t) + sizeof(ResponseMessage));

		rm = (ResponseMessage *) call Packet.getPayload(&message, sizeof(stats_t) + sizeof(ResponseMessage) );
	
		memcpy(rm->payload, &curr_stats, sizeof(stats_t));
		
		dbg("ApplicationDetails", "Request %d: Sending response (message len %d )\n", newVal->id, sizeof(stats_t));

		if ( !responseSendBusy) {
			error_t error;
			if ( (error = call Send.send(&message, sizeof(ResponseMessage) + sizeof(stats_t))) == SUCCESS) {
				responseSendBusy = TRUE;
			} else {
				dbg("Application", "Request %d: Failed to send response (error code %d)\n", newVal->id, error);
			}
		}
	}

	event void Request.changed() {
		
#ifdef TOSSIM
		const RequestMessage* newVal = call Request.get();
#endif

		if( !isRunning) return;
		
		dbg("Application", "Request %d: Received request\n", newVal->id);

		memcpy(&curr_stats, &stats, sizeof(stats_t));
		dbg("ApplicationDetails", "Scheduling next packet from changed\n");
		call BackoffTimer.startOneShot(call Random.rand16() % 1000);
	}
	

        event void BackoffTimer.fired() {
                sendPacket();
        }

	event void Send.sendDone(message_t *msg, error_t error) {
		responseSendBusy = FALSE;
		stats.sentPackets++;
	}

	command error_t StdControl.start() {
		if (isRunning) return SUCCESS;

		isRunning = TRUE;

		return SUCCESS;
	}

	command error_t StdControl.stop() {
		isRunning = FALSE;
		return SUCCESS;
	}

}
