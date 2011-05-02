/* Copyright (c) 2011 EPFL, Lausanne
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
#include <Timer.h>

module SinkP {
	provides interface StdControl;
	uses interface RootControl;
	uses interface Timer<TMilli> as RequestTimer;
	uses interface Receive;
	uses interface CollectionPacket;
	uses interface Packet;
	uses interface DisseminationUpdate<RequestMessage> as Request;
        uses interface SenseCodePacket;
} implementation {

	RequestMessage request;

	uint16_t last_request_id = -1; 

	bool isRunning = FALSE;

	int seconds = 0;

	int received[MAX_NODES];
	int count_received;
	int count_total;

	void printStats() {
		int i;

		for (i = 0 ; i < MAX_NODES ; i ++ ) {
			if ( received[i]) {
				dbg("ApplicationDetails", "Request %d: Estimate for source %d available (received %d)\n", last_request_id, i, received[i]);
			} else {
				dbg("ApplicationDetails", "Request %d: Estimate for source %d not available\n",last_request_id, i);
			}
		}

		dbg("Application", "Request %d: Estimated %d sources out of %d ( %d packets received )\n", last_request_id, count_received, MAX_NODES, count_total);
	}

	void clearStats() {
		int i;
		for (i = 0 ; i < MAX_NODES ; i ++ ) received[i] = 0;
		count_received = 0;
		count_total = 0;

	}

	command error_t StdControl.start() {
		if (isRunning) return SUCCESS;
		isRunning = TRUE;
		call RootControl.setRoot();            
		call RequestTimer.startPeriodic(1000);  		
		return SUCCESS;
	}

	command error_t StdControl.stop() {
		call RequestTimer.stop();
		isRunning = FALSE;
		return SUCCESS;
	}

	void startNewRequest() { 

		last_request_id++;

		request.id = last_request_id;
		request.redundancy = REDUNDANCY;
		request.coding = CODING;
		request.systematic = SYSTEMATIC_CODING;

		clearStats();

		dbg("Application", "%s: Starting request %d with schema %d\n", sim_time_string(), request.id, request.schema);	
		
		call Request.change( &request );
		
	}

	event void RequestTimer.fired() {        
		if ( !isRunning ) return;

		if ( seconds < SAMPLING_PERIOD_SEC  ) {
			seconds++;
			return;
		} else {
			seconds = 0;
		}

		if ( last_request_id != 0xFFFF) {
			printStats();
		}

		startNewRequest();
	}

#ifdef TOSSIM
        char* payloadDecode(message_t *msg) {
                static char data_str[500];
                char temp[10];
                int i;
                uint8_t* vector = (uint8_t*) call Packet.getPayload(msg, call Packet.payloadLength(msg));

                data_str[0] = 0;

                for ( i = 0 ; i < call Packet.payloadLength(msg) ; i++) {
                        sprintf(temp, "%02X", vector[i]);
                        strcat(data_str, temp);
                }

                return data_str;
        }

        char* vectorDecode(message_t *msg) {
                static char data_str[500];
                char temp[10];
                int i;
                uint8_t* vector = (uint8_t*) call SenseCodePacket.getCodingVectorData(msg);

                data_str[0] = 0;

                for ( i = 0 ; i < ( call SenseCodePacket.option(msg, CTP_OPT_CODED) ? MAX_CODING_VECTOR_LEN : 0) * 8 / 4 ; i++) {
                        sprintf(temp, "%x", ff_get_c(vector, i));
                        strcat(data_str, temp);
                }

                return data_str;
        }

#endif

	event message_t* Receive.receive(message_t* msg, void* payload, uint8_t len) {

		dbg("ApplicationReceived", "%s: Request %d: Received packet from %d, payload length %d, coded: %d, coding vector: %s payload: %s\n", sim_time_string(), call SenseCodePacket.getGeneration(msg), call CollectionPacket.getOrigin(msg), len, call SenseCodePacket.option(msg, CTP_OPT_CODED), vectorDecode(msg), payloadDecode(msg) );

		count_total++;
	
		if ( ! received[call CollectionPacket.getOrigin(msg)] ) {
			dbg("ApplicationReceived", "%s: Request %d: Received first packet from %d, payload length %d\n", sim_time_string(), call SenseCodePacket.getGeneration(msg), call CollectionPacket.getOrigin(msg), len);
			count_received++;
		}

		received[call CollectionPacket.getOrigin(msg)]++;

		return msg;
	}

}
