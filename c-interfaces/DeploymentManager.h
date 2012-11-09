#ifndef DEPLOYMENT_MANAGER
#define DEPLOYMENT_MANAGER

#include "SoOSiM.h"
#include "common.h"

// Depends on modules:
#include "CodeAdapter.h"
#include "MemoryManager.h"

class DeploymentManager
{
public:
  DeploymentManager();
  ~DeploymentManager();

  void deploy(code_t c, destination_t d);
  void migrate(code_t c, threadId tId, destination_t d);
};

#endif
