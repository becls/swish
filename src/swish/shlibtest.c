// Copyright 2018 Beckman Coulter, Inc.
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#define SWISH_IMPORT
#include "swish.h"
#undef EXPORT
#include <stdlib.h>

#ifdef __APPLE__
#include <malloc/malloc.h>
#else
#include <malloc.h>
#endif

#ifdef _WIN32
#define EXPORT __declspec (dllexport)
#else
#define EXPORT
#endif

#define container_of(p, t, member) ((t*)((char*)(p)-offsetof(t, member)))
#define malloc_container(t) ((t*)malloc(sizeof(t)))

EXPORT int square(int n) {
  return n * n;
}

typedef struct {
  uv_async_t async;
  ptr callback;
  ptr arg;
} call_req_t;

void close_cb(uv_handle_t* handle) {
  call_req_t* call_req = container_of(handle, call_req_t, async);
  free(call_req);
}

static void call_req_cb(uv_async_t* async) {
  call_req_t* call_req = container_of(async, call_req_t, async);
  ptr callback = call_req->callback;
  ptr arg = call_req->arg;
  Sunlock_object(callback);
  Sunlock_object(arg);
  osi_add_callback1(callback, Sstring(uv_err_name(UV_EROFS)));
  osi_add_callback1(callback, arg);
  uv_close((uv_handle_t*)&call_req->async, close_cb);
}

// make sure shared library can resolve functions from libuv, scheme, and osi
EXPORT ptr call_it(ptr cb, ptr arg) {
  call_req_t* req = malloc_container(call_req_t);
  if (!req)
    return osi_make_error_pair(__func__, UV_ENOMEM);
  int rc = uv_async_init(osi_loop, &req->async, call_req_cb);
  if (rc < 0)
    return osi_make_error_pair("uv_async_init", rc);
  rc = uv_async_send(&req->async);
  if (rc < 0)
    return osi_make_error_pair("uv_async_send", rc);
  Slock_object(cb);
  Slock_object(arg);
  req->callback = cb;
  req->arg = arg;
  return Strue;
}
