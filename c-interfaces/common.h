#ifndef COMMON_H
#define COMMON_H

#include "SoOSiM.h"

typedef componentId processId;
typedef processId threadId;

// Data structure representing code
struct code_t {};

// Data structure representing an hardware architecture
struct architecture_t {};

// Data structure representing 'data'
struct data_t {};

struct destination_t {
  int nodeId;
  architecture_t arch;
};

struct resourceDescription_t {};

#endif
