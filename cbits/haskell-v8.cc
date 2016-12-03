#include <stdlib.h>
#include "v8.h"
#include "libplatform/libplatform.h"

class ArrayBufferAllocator : public v8::ArrayBuffer::Allocator {
 public:
  virtual void* Allocate(size_t length) {
    void* data = AllocateUninitialized(length);
    return data == NULL ? data : memset(data, 0, length);
  }
  virtual void* AllocateUninitialized(size_t length) { return malloc(length); }
  virtual void Free(void* data, size_t) { free(data); }
};

extern "C" {
  v8::Platform* createDefaultPlatform() {
    return v8::platform::CreateDefaultPlatform();
  }

  void deletePlatform(v8::Platform* platform) {
    delete platform;
  }

  void shutdownPlatform() {
    v8::V8::ShutdownPlatform();
  }

	void initializePlatform(v8::Platform* platform) {
		v8::V8::InitializePlatform(platform);
	}

  void initialize() {
    v8::V8::Initialize();
  }

  v8::Isolate* newIsolate() {
    ArrayBufferAllocator allocator;
    v8::Isolate::CreateParams create_params;
    create_params.array_buffer_allocator = &allocator;
    return v8::Isolate::New(create_params);
  }

  void disposeIsolate(v8::Isolate* isolate) {
    isolate->Dispose();
  }

  char* evalStringInContext(v8::Isolate* isolate, char* utf8String) {
    v8::Isolate::Scope isolate_scope(isolate);
    v8::HandleScope handle_scope(isolate);
    v8::Local<v8::Context> context = v8::Context::New(isolate);
    v8::Context::Scope context_scope(context);
    v8::Local<v8::String> source =
      v8::String::NewFromUtf8(isolate, utf8String, v8::NewStringType::kNormal).ToLocalChecked();
    v8::Local<v8::Script> script = v8::Script::Compile(context, source).ToLocalChecked();
    v8::Local<v8::String> resultString =
      script->Run(context).ToLocalChecked()->ToString(context).ToLocalChecked();
    char *resultBuffer = (char *)malloc(resultString->Utf8Length() + 1);
    resultString->WriteUtf8(resultBuffer);
    return resultBuffer;
  }

  void freeString(char* string) {
    free(string);
  }
}
