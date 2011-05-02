from TOSSIM import *
import sys

maxValuesRead = 1000

# Create simulation, configure output
t = Tossim([])
times = [0.0 ,7000.0]
events = [[ 3 ] ,[ 2 ]]
numNodes = 32
t.addChannel("Application", sys.stdout)
t.addChannel("ApplicationReceived", sys.stdout)
t.addChannel("SenseCodePacket", sys.stdout)
# Create topology
print "Creating mote topology...";
sys.stdout.flush()
r = t.radio()
f = open("test/linkgains.dat", "r")
lines = f.readlines()
for line in lines:
  s = line.strip().split(" ")
  if (len(s) > 0):
    r.add(int(s[0]), int(s[1]), float(s[2]))
print "Created mote topology.";
sys.stdout.flush()

# Add statistical noise model to nodes
print "Creating noise models for the motes...";
sys.stdout.flush()
noise = open("test/noise.dat", "r")
lines = noise.readlines()
valuesRead = 0
for line in lines:
  st = line.strip()
  if (st != ""):
    val = int(st)
    for i in range(0, numNodes):
      t.getNode(i).addNoiseTraceReading(val)
    valuesRead += 1
    if valuesRead >= maxValuesRead:
      break
print "Data read preparing noise model"
sys.stdout.flush()
for i in range(0, numNodes):
  t.getNode(i).createNoiseModel()
print "Created noise models from", valuesRead, "trace readings.";
sys.stdout.flush()

# Run simulation
print "Running the simulation...";
t.runNextEvent()
time = t.time()
prev = t.time()
while True:
  if ( (t.time() - prev) > t.ticksPerSecond() ) :
        prev = t.time()
        print  "TIME", (t.time() / ( t.ticksPerSecond() / 1000) ) 
        sys.stdout.flush()
  while times[0] * t.ticksPerSecond() <= t.time():
        print "Running event with %f secs of delay" % (times[0] - t.time() / t.ticksPerSecond() ) 
        if events[0][0] == 0:
          print("DEBUG (" + str(events[0][1]) + "): " + str(t.time()) + ": Powering on node")
          t.getNode(events[0][1]).turnOn()
          t.getNode(events[0][1]).bootAtTime(t.time() + 1)
        elif events[0][0] == 1:
          print("DEBUG (" + str(events[0][1]) + "): " + str(t.time()) + ": Powering off node")
          t.getNode(events[0][1]).turnOff()
        elif events[0][0] == 2:
          print "DEBUG (0): " + str(t.time()) + ": Stopping simulation" 
          exit(0)
        elif events[0][0] == 3:
          for i in range(0, numNodes):
              print("DEBUG (" + str(i) + "): " + str(t.time()) + ": Powering on node")
              t.getNode(i).turnOn()
              t.getNode(i).bootAtTime(t.time() + 1)
        del times[0]
        del events[0]
        sys.stdout.flush()
  t.runNextEvent()
print "Simulation over."
sys.stdout.flush()
