COMPONENT=DemoAppC
CFLAGS += -I./sensecode  -I$(TOSDIR)/lib/net/le -I.
CFLAGS += -I$(TOSDIR)/lib/net/drip
CFLAGS += -I$(TOSDIR)/lib/net -I/usr/include/python2.4/ -I$(TOSDIR)/lib/safe/include
CFLAGS += -D'TOSH_DATA_LENGTH=40'

include $(MAKERULES)

