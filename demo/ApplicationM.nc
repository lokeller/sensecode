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
module ApplicationM {

   uses interface Boot;
   uses interface SplitControl as RadioControl;
   uses interface StdControl as SourceControl;
   uses interface StdControl as SinkControl;
   uses interface StdControl as RoutingControl;
   uses interface StdControl as RequestControl;
   uses interface StdControl as DisseminationControl;

} implementation {

   event void Boot.booted() {
	call RadioControl.start();
   }

   event void RadioControl.startDone(error_t result) {
      if (result != SUCCESS)
         call RadioControl.start();
      else {
         call RoutingControl.start();
	 call DisseminationControl.start();
	 if (TOS_NODE_ID == 0 ) { 
		 call SinkControl.start();
	 } else {
		 call SourceControl.start();
	 }
      } 
   }

   event void RadioControl.stopDone(error_t result) {}

}
