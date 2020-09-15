#include <stdio.h>
#include "solaris.h"

/*
  Getting CPU Usage from the /proc
*/
static int read_buffer(char *buffer, int buffer_size, char* file_path) {
  FILE *fp = fopen(file_path, "r");
  if(fp != NULL) {
    size_t read_len = fread(buffer, sizeof(char), buffer_size, fp);
    if(read_len == 0) {
      return 0;
    }

    fclose(fp);
    return 1;
  } else {
    return 0;
  }
}

static char* get_proc_file(char* type, char* pid) {
  size_t type_len = strlen(type);
  size_t pid_len = strlen(pid);
  size_t proc_len = strlen("/proc/");
  size_t spaces = 2;
  size_t slashes = 1;

  size_t total = proc_len + pid_len + type_len + spaces + slashes + 1;
  char* final = (char*)malloc(total);

  sprintf(final, "/proc/%s/%s", pid, type);
  return final;
}

static float get_pct(ushort_t pct) {
  uint_t value = pct; /* need 32 bits to compute with */
  value = ((value * 1000) + 0x7000) >> 15;    /* [0 .. 1000] */

  float f_value = (float)value;
  return f_value/10;
}

int get_usage(int pid, ps_usage_t* ps_usage) {
  char str_pid[10];
  sprintf(str_pid, "%d", pid);

  char* psinfo_proc_file = get_proc_file("psinfo", str_pid);
  psinfo_t psinfo;
  int success_psinfo = read_buffer((char*)&psinfo, sizeof(psinfo), psinfo_proc_file);
  free(psinfo_proc_file);

  if(success_psinfo) {
    ps_usage->cpu = get_pct(psinfo.pr_pctcpu);
    ps_usage->memory = psinfo.pr_rssize * 1024;
    return 1;
  } else {
    return 0;
  }
}

/*
    V8 Integration
*/

void AsyncGetUsage(uv_work_t* req) {
  UsageData* usageData = static_cast<UsageData*>(req->data);
  ps_usage_t ps_usage;
  int pid_exists = get_usage(usageData->pid, &ps_usage);

  if(pid_exists) {
    usageData->ps_usage = ps_usage;
  } else {
    usageData->failed = true;
  }
}

void AsyncAfterGetUsage(uv_work_t* req) {
  UsageData* usageData = static_cast<UsageData*>(req->data);

  if(usageData->failed) {
    const unsigned argc = 1;
    Local<Value> argv[argc] = {
      Exception::Error(String::New("INVALID_PID"))
    };
    usageData->callback->Call(Context::GetCurrent()->Global(), argc, argv);
  } else {
    Local<Object> usage = Object::New();
    usage->Set(String::NewSymbol("cpu"), Number::New(usageData->ps_usage.cpu));
    usage->Set(String::NewSymbol("memory"), Number::New(usageData->ps_usage.memory));

    const unsigned argc = 2;
    Local<Value> argv[argc] = { Local<Value>::New(Null()), usage };
    usageData->callback->Call(Context::GetCurrent()->Global(), argc, argv);
  }

  delete usageData;
  delete req;
}

Handle<Value> GetUsage(const Arguments& args) {
  HandleScope scope;

  int pid = (int)(Local<Number>::Cast(args[0])->Value());
  Local<Function> callback = Local<Function>::Cast(args[1]);

  UsageData* usageData = new UsageData();
  usageData->pid = pid;
  usageData->callback = Persistent<Function>::New(callback);
  usageData->failed = false;

  uv_work_t* req = new uv_work_t();
  req->data = usageData;

  uv_queue_work(uv_default_loop(), req, AsyncGetUsage, (uv_after_work_cb)AsyncAfterGetUsage);

  return scope.Close(Undefined());
}
