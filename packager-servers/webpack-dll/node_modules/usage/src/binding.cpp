#include "binding.h"
#include "nan.h"
using namespace v8;

void RegisterModule(Local<Object> exports) {

#ifdef is_linux
  exports->Set(Nan::New<String>("HERTZ").ToLocalChecked(), Nan::New<Number>(sysconf(_SC_CLK_TCK)));
  exports->Set(Nan::New<String>("PAGE_SIZE").ToLocalChecked(), Nan::New<Number>(sysconf(_SC_PAGESIZE)));
#endif

#ifdef is_solaris
  exports->Set(Nan::New<String>("getUsage").ToLocalChecked(),
    FunctionTemplate::New(GetUsage)->GetFunction());
#endif
  exports->Set(Nan::New<String>("OS").ToLocalChecked(), Nan::New<String>(OS).ToLocalChecked());
}

NODE_MODULE(sysinfo, RegisterModule);
