// Get the unique id of your component
int getComponentId();

// Get the unique id of the node that is hosting your component
int getNodeId();

// Get the current simulation cycle
int getTime();

// Write memory of local node
void writeMemory(int address, char* val);

// Read memory of local node
char* readMemory(int address);
