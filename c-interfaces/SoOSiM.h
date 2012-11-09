#ifndef SOOSIM_H
#define SOOSIM_H

typedef int nodeId;
typedef int componentId;

// Get the unique id of your component
componentId getComponentId();

// Get the unique id of the node that is hosting your component
nodeId getNodeId();

// Get the current simulation cycle
int getTime();

// Write memory of local node
void writeMemory(long address, char* val);

// Read memory of local node
char* readMemory(long address);

#endif
