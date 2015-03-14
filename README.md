SenseCode: network coding for reliable sensor networks
-----------------------------------------------------------------

**WARNING: This implementation of SenseCode is a prototype and therefore may 
contain serious bugs.**

This is an implementation of the protocol describe in the paper:

[L. Keller, E. Atsan, K. Argyraki, and C. Fragouli,
 SenseCode: Network Coding for Reliable Sensor Networks, 
 in ACM Transactions on Sensor Networks, vol. 9, iss. 2, 2013.](http://lorenzo.nodo.ch/media/files/papers/tosn2013-sensecode.pdf)

**Abstract**:

Designing a communication protocol for sensor networks often involves obtaining 
the "right" trade-off between energy efficiency and end-to-end packet error rate. 
In this paper, we show that network coding provides a means to elegantly balance 
these two goals. We present the design and implementation of SenseCode, a collection 
protocol for sensor networks and - , to the best of our knowledge, the first 
such implemented protocol to employ network coding. SenseCode provides a way to 
gracefully introduce a configurable amount of redundant information in the network, 
thereby increasing end-to-end packet error rate in the face of packet loss. We 
compare SenseCode to the best (to our knowledge) existing alternative and show that
it reduces end-to-end packet error rate in highly dynamic environments, while consuming 
a comparable amount of network resources. We have implemented SenseCode as 
a TinyOS module and evaluate it through extensive TOSSIM simulations. 

**Description of the contents**:

The implementation of SenseCode is included in the `sensecode` directory.

The directory demo contains a demo application that uses SenseCode.

The demo application consists of a sink node that every 60 seconds broadcasts a request to 
all nodes in the network. All the other nodes answer the request by sending a packet with SenseCode.

The demo application is setup to send data with a rate 0.5 systematic code and support 
up to 32 active nodes. It is possible to change these parameters in the file  `DemoApp.h`.

Included in the demo directory there is also a python script to run a TOSSIM simulation with 32 nodes.

To run the demo application with TOSSIM:

  1. Run `make micaz sim` in the `demo` directory
  2. Run `python runner.py` in the `demo` directory

