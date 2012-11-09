#ifndef MEMORY_MANAGER
#define MEMORY_MANAGER

#include "SoOSiM.h"
#include "common.h"
#include <vector>

// Depends on modules:
#include "ResourceDiscovery.h"
#include "ProcessManager.h"

struct MemoryTableEntry
{
  int processID;
  long virtualBaseAddress;
  long size;
  std::vector<nodeId> hosts;
};

class MemoryManager
{
public:
  MemoryManager();
  ~MemoryManager();

  long allocate(int procId, long size, resourceDescription_t rd );
  bool reserve(int procId, long vmemAddr, long size);
  std::vector<MemoryTableEntry> getentries( int );
  nodeId resolve(processId pId, long memAddr);
  char* read(processId pId, long memAddr, int size);
  void write(processId pId, long memAddr, int size, char* val);
};

#endif
