#ifndef RESOURCE_DISCOVERY
#define RESOURCE_DISCOVERY

#include "SoOSiM.h"
#include "common.h"
#include <vector>
#include <string>

class ResourceDiscovery
{
public:
  ResourceDiscovery();
  ~ResourceDiscovery();

  std::vector<nodeId> findResources(std::string);
};

#endif
