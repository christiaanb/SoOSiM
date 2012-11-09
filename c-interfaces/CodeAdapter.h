#ifndef CODE_ADAPTER
#define CODE_ADAPTER

#include "SoOSiM.h"
#include "common.h"

struct adaptedCode_t {};
struct standarData_t {};
struct nativeData_t  {};

class CodeAdapter
{
public:
  CodeAdapter();
  ~CodeAdapter();

  adaptedCode_t adaptCode (code_t code, architecture_t destination);
  standarData_t dataStandardisation (data_t d);
  nativeData_t dataNativisation (data_t d);
};

#endif
