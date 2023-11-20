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

#include "osi.h"

#include <assert.h>

#ifdef _WIN32
#pragma comment(lib, "advapi32.lib")
#pragma comment(lib, "iphlpapi.lib")
#pragma comment(lib, "psapi.lib")
#pragma comment(lib, "rpcrt4.lib")
#pragma comment(lib, "user32.lib")
#pragma comment(lib, "userenv.lib")
#pragma comment(lib, "winmm.lib")
#pragma comment(lib, "ws2_32.lib")
typedef DWORD osi_thread_t;
#else
#include <pthread.h>
typedef pthread_t osi_thread_t;
#endif

typedef struct {
  osi_port_vtable_t* vtable;
  uv_file file;
} fs_port_t;

typedef struct {
  osi_port_vtable_t* vtable;
  ptr close_callback;
  ptr read_buffer;
  size_t read_start_index;
  uint32_t read_size;
  ptr read_callback;
  uv_write_t write_req;
  ptr write_buffer;
  ptr write_buffer2;
  uint32_t write_size;
  ptr write_callback;
  union {
    uv_handle_t handle;
    uv_stream_t stream;
    uv_pipe_t pipe;
    uv_tcp_t tcp;
  } h;
} stream_port_t;

typedef struct {
  uv_fs_t fs;
  ptr buffer;
  ptr callback;
} rw_fs_req_t;

typedef struct {
  uv_fs_t fs;
  fs_port_t* port;
  ptr callback;
} close_fs_req_t;

typedef struct {
  ptr callback;
  uv_getaddrinfo_t getaddrinfo;
  struct addrinfo* ai;
  stream_port_t* stream_port;
  uv_connect_t connect;
} tcp_connect_t;

typedef struct {
  uv_async_t async;
  uv_mutex_t mutex;
  void (*code)(void*);
  void* payload;
  uv_cond_t cond;
  uv_cond_t* sender;
} send_request_t;

uv_loop_t* osi_loop;

jmp_t g_exit;
int g_is_service;

static uint64_t g_threshold;
static ptr g_callbacks;

static send_request_t g_send_request;
static osi_thread_t g_scheme_thread;

void osi_add_callback_list(ptr callback, ptr args) {
  assert(g_callbacks); // must only be called via osi_get_callbacks
  g_callbacks = Scons(Scons(callback, args), g_callbacks);
}

void osi_add_callback1(ptr callback, ptr arg) {
  osi_add_callback_list(callback, Scons(arg, Snil));
}

void osi_add_callback2(ptr callback, ptr arg1, ptr arg2) {
  osi_add_callback_list(callback, Scons(arg1, Scons(arg2, Snil)));
}

void osi_add_callback3(ptr callback, ptr arg1, ptr arg2, ptr arg3) {
  osi_add_callback_list(callback, Scons(arg1, Scons(arg2, Scons(arg3, Snil))));
}

ptr osi_make_error_pair(const char* who, int error) {
  return Scons(Sstring_to_symbol(who), Sinteger(error));
}

char* osi_string_to_utf8(ptr s, size_t* utf8_len) {
  // utf8_len does not include the nul terminator character.
  size_t n = Sstring_length(s);
  size_t len = 0;
  for (size_t i = 0; i < n; i++) {
    string_char c = Sstring_ref(s, i);
    if (c < 0x80)
      len += 1;
    else if (c < 0x800)
      len += 2;
    else if (c < 0x10000)
      len += 3;
    else
      len += 4;
  }
  char* result = (char*)malloc(len + 1);
  if (!result) {
    *utf8_len = 0;
    return NULL;
  }
  char* utf8 = result;
  for (size_t i = 0; i < n; i++) {
    string_char c = Sstring_ref(s, i);
    if (c < 0x80)
      *utf8++ = c;
    else if (c < 0x800) {
      *utf8++ = (c >> 6) | 0xC0;
      *utf8++ = (c & 0x3F) | 0x80;
    } else if (c < 0x10000) {
      *utf8++ = (c >> 12) | 0xE0;
      *utf8++ = ((c >> 6) & 0x3F) | 0x80;
      *utf8++ = (c & 0x3F) | 0x80;
    } else {
      *utf8++ = (c >> 18) | 0xF0;
      *utf8++ = ((c >> 12) & 0x3F) | 0x80;
      *utf8++ = ((c >> 6) & 0x3F) | 0x80;
      *utf8++ = (c & 0x3F) | 0x80;
    }
  }
  *utf8 = 0;
  *utf8_len = len;
  return result;
}

static void rw_fs_cb(uv_fs_t* req) {
  ssize_t result = req->result;
  rw_fs_req_t* fs_req = container_of(req, rw_fs_req_t, fs);
  ptr buffer = fs_req->buffer;
  ptr callback = fs_req->callback;
  uv_fs_req_cleanup(req);
  Sunlock_object(buffer);
  Sunlock_object(callback);
  free(fs_req);
  osi_add_callback1(callback, Sinteger(result));
}

static ptr read_fs_port(uptr port, ptr buffer, size_t start_index, uint32_t size, int64_t offset, ptr callback) {
  fs_port_t* p = (fs_port_t*)port;
  rw_fs_req_t* req = malloc_container(rw_fs_req_t);
  if (!req)
    return osi_make_error_pair("osi_read_port", UV_ENOMEM);
  Slock_object(buffer);
  Slock_object(callback);
  req->buffer = buffer;
  req->callback = callback;
  uv_buf_t buf = {
    .base = (char*)& Sbytevector_u8_ref(buffer, start_index),
    .len = size
  };
  int rc = uv_fs_read(osi_loop, &req->fs, p->file, &buf, 1, offset, rw_fs_cb);
  if (rc < 0) {
    Sunlock_object(buffer);
    Sunlock_object(callback);
    free(req);
    return osi_make_error_pair("uv_fs_read", rc);
  }
  return Strue;
}

static ptr write_fs_port(uptr port, ptr buffer, size_t start_index, uint32_t size, int64_t offset, ptr callback) {
  fs_port_t* p = (fs_port_t*)port;
  rw_fs_req_t* req = malloc_container(rw_fs_req_t);
  if (!req)
    return osi_make_error_pair("osi_write_port", UV_ENOMEM);
  Slock_object(buffer);
  Slock_object(callback);
  req->buffer = buffer;
  req->callback = callback;
  uv_buf_t buf = {
    .base = (char*)& Sbytevector_u8_ref(buffer, start_index),
    .len = size
  };
  int rc = uv_fs_write(osi_loop, &req->fs, p->file, &buf, 1, offset, rw_fs_cb);
  if (rc < 0) {
    Sunlock_object(buffer);
    Sunlock_object(callback);
    free(req);
    return osi_make_error_pair("uv_fs_write", rc);
  }
  return Strue;
}

static void close_fs_cb(uv_fs_t* req) {
  ssize_t result = req->result;
  close_fs_req_t* fs_req = container_of(req, close_fs_req_t, fs);
  fs_port_t* port = fs_req->port;
  ptr callback = fs_req->callback;
  uv_fs_req_cleanup(req);
  Sunlock_object(callback);
  free(port);
  free(req);
  osi_add_callback1(callback, Sinteger(result));
}

static ptr close_fs_port(uptr port, ptr callback) {
  fs_port_t* p = (fs_port_t*)port;
  close_fs_req_t* req = malloc_container(close_fs_req_t);
  if (!req)
    return osi_make_error_pair("osi_close_port", UV_ENOMEM);
  Slock_object(callback);
  req->port = p;
  req->callback = callback;
  int rc = uv_fs_close(osi_loop, &req->fs, p->file, close_fs_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return osi_make_error_pair("uv_fs_close", rc);
  }
  return Strue;
}

static void close_async_cb(uv_handle_t* handle) {
  free(handle);
}

static void close_fd_cb(uv_async_t* async) {
  ptr callback = (ptr)async->data;
  Sunlock_object(callback);
  uv_close((uv_handle_t*)async, close_async_cb);
  osi_add_callback1(callback, Sfixnum(0));
}

static ptr close_fd_port(uptr port, ptr callback) {
  // Use an async handle to add the callback in the proper context.
  uv_async_t* async = malloc_container(uv_async_t);
  if (!async)
    return osi_make_error_pair("osi_close_fd_port", UV_ENOMEM);
  int rc = uv_async_init(osi_loop, async, close_fd_cb);
  if (rc < 0)
    return osi_make_error_pair("uv_async_init", rc);
  rc = uv_async_send(async);
  if (rc < 0)
    return osi_make_error_pair("uv_async_send", rc);
  Slock_object(callback);
  async->data = callback;
  fs_port_t* p = (fs_port_t*)port;
  free(p);
  return Strue;
}

static osi_port_vtable_t fd_vtable = {
  .close = close_fd_port,
  .read = read_fs_port,
  .write = write_fs_port
};

static osi_port_vtable_t file_vtable = {
  .close = close_fs_port,
  .read = read_fs_port,
  .write = write_fs_port
};

static void open_fs_cb(uv_fs_t* req) {
  ssize_t result = req->result;
  ptr callback = (ptr)req->data;
  uv_fs_req_cleanup(req);
  Sunlock_object(callback);
  free(req);
  if (result < 0) {
    osi_add_callback1(callback, osi_make_error_pair("uv_fs_open", (int)result));
    return;
  }
  fs_port_t* port = malloc_container(fs_port_t);
  if (!port) {
    osi_add_callback1(callback, osi_make_error_pair("uv_fs_open", UV_ENOMEM));
    return;
  }
  port->vtable = &file_vtable;
  port->file = (uv_file)result;
  osi_add_callback1(callback, Sunsigned((uptr)port));
}

static void get_file_size_cb(uv_fs_t* req) {
  ptr callback = (ptr)req->data;
  if (req->result < 0)
    osi_add_callback1(callback, osi_make_error_pair("uv_fs_fstat", (int)req->result));
  else
    osi_add_callback1(callback, Sunsigned64(req->statbuf.st_size));
  uv_fs_req_cleanup(req);
  Sunlock_object(callback);
  free(req);
}

static void get_real_path_cb(uv_fs_t* req) {
  ptr callback = (ptr)req->data;
  if (req->result < 0)
    osi_add_callback1(callback, osi_make_error_pair("uv_fs_realpath", (int)req->result));
  else
    osi_add_callback1(callback, Sstring_utf8(req->ptr, -1));
  uv_fs_req_cleanup(req);
  Sunlock_object(callback);
  free(req);
}

static void return_fs_result_cb(uv_fs_t* req) {
  ptr callback = (ptr)req->data;
  osi_add_callback1(callback, Sinteger(req->result));
  uv_fs_req_cleanup(req);
  Sunlock_object(callback);
  free(req);
}

static ptr make_time(const uv_timespec_t* ts) {
  return Scons(Sinteger32(ts->tv_sec), Sinteger32(ts->tv_nsec));
}

static void stat_cb(uv_fs_t* req) {
  ptr callback = (ptr)req->data;
  if (req->result < 0)
    osi_add_callback1(callback, Sinteger(req->result));
  else {
    ptr r = Smake_vector(17, Sfixnum(0));
    Svector_set(r, 0, Sstring_to_symbol("<stat>"));
    Svector_set(r, 1, Sunsigned64(req->statbuf.st_dev));
    Svector_set(r, 2, Sunsigned64(req->statbuf.st_mode));
    Svector_set(r, 3, Sunsigned64(req->statbuf.st_nlink));
    Svector_set(r, 4, Sunsigned64(req->statbuf.st_uid));
    Svector_set(r, 5, Sunsigned64(req->statbuf.st_gid));
    Svector_set(r, 6, Sunsigned64(req->statbuf.st_rdev));
    Svector_set(r, 7, Sunsigned64(req->statbuf.st_ino));
    Svector_set(r, 8, Sunsigned64(req->statbuf.st_size));
    Svector_set(r, 9, Sunsigned64(req->statbuf.st_blksize));
    Svector_set(r, 10, Sunsigned64(req->statbuf.st_blocks));
    Svector_set(r, 11, Sunsigned64(req->statbuf.st_flags));
    Svector_set(r, 12, Sunsigned64(req->statbuf.st_gen));
    Svector_set(r, 13, make_time(&(req->statbuf.st_atim)));
    Svector_set(r, 14, make_time(&(req->statbuf.st_mtim)));
    Svector_set(r, 15, make_time(&(req->statbuf.st_ctim)));
    Svector_set(r, 16, make_time(&(req->statbuf.st_birthtim)));
    osi_add_callback1(callback, r);
  }
  uv_fs_req_cleanup(req);
  Sunlock_object(callback);
  free(req);
}

static void list_directory_cb(uv_fs_t* req) {
  ptr callback = (ptr)req->data;
  if (req->result < 0)
    osi_add_callback1(callback, Sinteger(req->result));
  else {
    ptr ls = Snil;
    uv_dirent_t ent;
    while (uv_fs_scandir_next(req, &ent) >= 0)
      ls = Scons(Scons(Sstring_utf8(ent.name, -1), Sinteger(ent.type)), ls);
    osi_add_callback1(callback, ls);
  }
  uv_fs_req_cleanup(req);
  Sunlock_object(callback);
  free(req);
}

static void read_stream_alloc_cb(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf) {
  (void)suggested_size;
  stream_port_t* p = container_of(handle, stream_port_t, h.stream);
  buf->base = (char*)&Sbytevector_u8_ref(p->read_buffer, p->read_start_index);
  buf->len = p->read_size;
}

static void read_stream_cb(uv_stream_t* stream, ssize_t nread, const uv_buf_t* buf) {
  (void)buf;
  uv_read_stop(stream);
  stream_port_t* p = container_of(stream, stream_port_t, h.stream);
  ptr callback = p->read_callback;
  Sunlock_object(p->read_buffer);
  Sunlock_object(callback);
  p->read_callback = 0;
  osi_add_callback1(callback, Sinteger(nread));
}

static ptr read_stream_port(uptr port, ptr buffer, size_t start_index, uint32_t size, int64_t offset, ptr callback) {
  if (-1 != offset)
    return osi_make_error_pair("osi_read_port", UV_EINVAL);
  stream_port_t* p = (stream_port_t*)port;
  if (p->read_callback)
    return osi_make_error_pair("osi_read_port", UV_EBUSY);
  Slock_object(buffer);
  Slock_object(callback);
  p->read_buffer = buffer;
  p->read_start_index = start_index;
  p->read_size = size;
  p->read_callback = callback;
  int rc = uv_read_start(&(p->h.stream), read_stream_alloc_cb, read_stream_cb);
  if (rc < 0) {
    Sunlock_object(buffer);
    Sunlock_object(callback);
    p->read_callback = 0;
    return osi_make_error_pair("uv_read_start", rc);
  }
  return Strue;
}

static void write_stream_cb(uv_write_t* req, int status) {
  stream_port_t* p = container_of(req, stream_port_t, write_req);
  ptr callback = p->write_callback;
  Sunlock_object(p->write_buffer);
  if (p->write_buffer2) {
    Sunlock_object(p->write_buffer2);
    p->write_buffer2 = 0;
  }
  Sunlock_object(callback);
  p->write_callback = 0;
  osi_add_callback1(callback, (status < 0) ? Sinteger32(status) : Sunsigned32(p->write_size));
}

static ptr write_stream_port(uptr port, ptr buffer, size_t start_index, uint32_t size, int64_t offset, ptr callback) {
  if (-1 != offset)
    return osi_make_error_pair("osi_write_port", UV_EINVAL);
  stream_port_t* p = (stream_port_t*)port;
  if (p->write_callback)
    return osi_make_error_pair("osi_write_port", UV_EBUSY);
  Slock_object(buffer);
  Slock_object(callback);
  p->write_buffer = buffer;
  p->write_buffer2 = 0;
  p->write_size = size;
  p->write_callback = callback;
  uv_buf_t buf = {
    .base = (char*)& Sbytevector_u8_ref(p->write_buffer, start_index),
    .len = size
  };
  int rc = uv_write(&(p->write_req), &(p->h.stream), &buf, 1, write_stream_cb);
  if (rc < 0) {
    Sunlock_object(buffer);
    Sunlock_object(callback);
    p->write_callback = 0;
    return osi_make_error_pair("uv_write", rc);
  }
  return Strue;
}

ptr osi_tcp_write2(uptr port, ptr bv1, ptr bv2, size_t start_index2, uint32_t size2, ptr callback) {
  size_t last2 = start_index2 + size2;
  size_t total_size = Sbytevector_length(bv1) + size2;
  if (!Sbytevectorp(bv1) || !Sbytevectorp(bv2) || !Sprocedurep(callback) ||
      // zero size2 is okay
      (last2 < start_index2) || // start_index2 + size2 overflowed
      (last2 > (size_t)Sbytevector_length(bv2)) ||
      (total_size > UINT32_MAX))
    return osi_make_error_pair("osi_tcp_write2", UV_EINVAL);
  stream_port_t* p = (stream_port_t*)port;
  if (p->write_callback)
    return osi_make_error_pair("osi_tcp_write2", UV_EBUSY);
  Slock_object(bv1);
  Slock_object(bv2);
  Slock_object(callback);
  p->write_buffer = bv1;
  p->write_buffer2 = bv2;
  p->write_size = (uint32_t)total_size;
  p->write_callback = callback;
  uv_buf_t buf[] = {
    {
      .base = (char*)& Sbytevector_u8_ref(bv1, 0),
      .len = (uint32_t)Sbytevector_length(bv1)
    },
    {
      .base = (char*)& Sbytevector_u8_ref(bv2, start_index2),
      .len = size2
    }
  };
  int rc = uv_write(&(p->write_req), &(p->h.stream), buf, 2, write_stream_cb);
  if (rc < 0) {
    Sunlock_object(bv1);
    Sunlock_object(bv2);
    Sunlock_object(callback);
    p->write_callback = 0;
    p->write_buffer2 = 0;
    return osi_make_error_pair("uv_write", rc);
  }
  return Strue;
}

static void close_stream_cb(uv_handle_t* handle) {
  // This code does not check and unlock the write_callback and
  // write_buffer because LibUV explicitly cancels write operations
  // with UV_ECANCELED before processing this callback.
  stream_port_t* p = container_of(handle, stream_port_t, h.stream);
  ptr callback = p->close_callback;
  ptr read_callback = p->read_callback;
  ptr read_buffer = p->read_buffer;
  free(p);
  if (read_callback) {
    Sunlock_object(read_buffer);
    Sunlock_object(read_callback);
    osi_add_callback1(read_callback, Sfixnum(UV_EOF));
  }
  if (callback) {
    Sunlock_object(callback);
    osi_add_callback1(callback, Sfixnum(0));
  }
}

static ptr close_stream_port(uptr port, ptr callback) {
  stream_port_t* p = (stream_port_t*)port;
  if (p->close_callback)
    return osi_make_error_pair("osi_close_port", UV_EBUSY);
  Slock_object(callback);
  p->close_callback = callback;
  uv_close(&(p->h.handle), close_stream_cb);
  return Strue;
}

static osi_port_vtable_t pipe_vtable = {
  .close = close_stream_port,
  .read = read_stream_port,
  .write = write_stream_port
};

static osi_port_vtable_t tcp_vtable = {
  .close = close_stream_port,
  .read = read_stream_port,
  .write = write_stream_port
};

static void connect_tcp_cb(uv_connect_t* req, int status) {
  tcp_connect_t* p = container_of(req, tcp_connect_t, connect);
  if (status < 0) {
    do {
      struct addrinfo* ai = p->ai;
      if (!ai) {
        ptr callback = p->callback;
        Sunlock_object(callback);
        uv_close(&(p->stream_port->h.handle), close_stream_cb);
        uv_freeaddrinfo(p->getaddrinfo.addrinfo);
        free(p);
        osi_add_callback1(callback, osi_make_error_pair("uv_tcp_connect", status));
        return;
      }
      p->ai = ai->ai_next;
      status = uv_tcp_connect(&(p->connect), &(p->stream_port->h.tcp), ai->ai_addr, connect_tcp_cb);
    } while (status < 0);
    return;
  }
  ptr callback = p->callback;
  stream_port_t* port = p->stream_port;
  Sunlock_object(callback);
  uv_freeaddrinfo(p->getaddrinfo.addrinfo);
  free(p);
  osi_add_callback1(callback, Sunsigned((uptr)port));
}

static void connect_tcp_addrinfo_cb(uv_getaddrinfo_t* req, int status, struct addrinfo* res) {
  tcp_connect_t* p = container_of(req, tcp_connect_t, getaddrinfo);
  if (status < 0) {
    ptr callback = p->callback;
    Sunlock_object(callback);
    uv_close(&(p->stream_port->h.handle), close_stream_cb);
    free(p);
    osi_add_callback1(callback, osi_make_error_pair("uv_getaddrinfo", status));
    return;
  }
  p->ai = res->ai_next;
  int rc = uv_tcp_connect(&(p->connect), &(p->stream_port->h.tcp), res->ai_addr, connect_tcp_cb);
  if (rc < 0)
    connect_tcp_cb(&(p->connect), rc);
}

static void close_handle_data_cb(uv_handle_t* handle) {
  ptr callback = (ptr)handle->data;
  free(handle);
  if (callback)
    Sunlock_object(callback);
}

static void listen_tcp_cb(uv_stream_t* server, int status) {
  ptr callback = (ptr)server->data;
  if (status < 0) {
    osi_add_callback1(callback, osi_make_error_pair("uv_listen", status));
    return;
  }
  stream_port_t* port = malloc_container(stream_port_t);
  if (!port) {
    osi_add_callback1(callback, osi_make_error_pair("uv_listen", UV_ENOMEM));
    return;
  }
  status = uv_tcp_init(osi_loop, &(port->h.tcp));
  if (status < 0) {
    free(port);
    osi_add_callback1(callback, osi_make_error_pair("uv_tcp_init", status));
    return;
  }
  port->vtable = &tcp_vtable;
  port->close_callback = 0;
  port->read_callback = 0;
  port->write_callback = 0;
  status = uv_accept(server, &(port->h.stream));
  if (status < 0) {
    uv_close(&(port->h.handle), close_stream_cb);
    osi_add_callback1(callback, osi_make_error_pair("uv_accept", status));
    return;
  }
  osi_add_callback1(callback, Sunsigned((uptr)port));
}

static void watch_path_cb(uv_fs_event_t* handle, const char* filename, int events, int status) {
  ptr callback = (ptr)handle->data;
  if (status < 0) {
    osi_add_callback1(callback, Sinteger(status));
    return;
  }
  osi_add_callback2(callback, filename ? Sstring_utf8(filename, -1) : Sfalse, Sinteger(events));
}

ptr osi_tcp_nodelay(uptr port, int enable) {
  stream_port_t* p = (stream_port_t*)port;
  if (p->vtable != &tcp_vtable)
    return osi_make_error_pair("uv_tcp_nodelay", UV_EINVAL);
  int rc = uv_tcp_nodelay(&(p->h.tcp), enable);
  return (rc < 0) ? Sfalse : Strue;
}

static int string_list_length(ptr x) {
  int n = 0;
  while (Spairp(x)) {
    if (!Sstringp(Scar(x)))
      return -1;
    ++n;
    x = Scdr(x);
  }
  if (!Snullp(x))
    return -1;
  return n;
}

static void free_argv(char** argv) {
  for (char** arg = argv + 1; *arg; arg++)
    free(*arg);
  free(argv);
}

static void process_exit_cb(uv_process_t* process, int64_t exit_status, int term_signal) {
  ptr callback = (ptr)process->data;
  if (callback) {
    osi_add_callback3(callback, Sinteger32(process->pid), Sinteger64(exit_status), Sinteger32(term_signal));
    Sunlock_object(callback);
    process->data = 0;
  }
  uv_close((uv_handle_t*)process, close_handle_data_cb);
}

static void list_uv_cb(uv_handle_t* handle, void* arg) {
  ptr* pls = (ptr*)arg;
  *pls = Scons(Scons(Sunsigned((uptr)handle), Sinteger32(handle->type)), *pls);
}

static int g_argc = 0;
static const char** g_argv;
void osi_set_argv(int argc, const char* argv[]) {
  g_argc = argc;
  g_argv = argv;
}

ptr osi_get_argv() {
  ptr argv = Smake_vector(g_argc, Sfalse);
  for (int i = 0; i < g_argc; i++) {
    Svector_set(argv, i, Sstring_utf8(g_argv[i], -1));
  }
  return argv;
}

size_t osi_get_bytes_used(void) {
#if defined(__APPLE__)
  struct mstats ms = mstats();
  return ms.bytes_used;
#elif defined(__GLIBC__)
#if (__GLIBC__ > 2) || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 33)
  struct mallinfo2 hinfo = mallinfo2();
#else
  struct mallinfo hinfo = mallinfo();
#endif
  return hinfo.hblkhd + hinfo.uordblks;
#elif defined(_WIN32)
  size_t used = 0;
  _HEAPINFO hinfo = { ._pentry = NULL };
  while (_heapwalk(&hinfo) == _HEAPOK)
    if (_USEDENTRY == hinfo._useflag)
      used += hinfo._size;
  return used;
#else
  return 0;
#endif
}

ptr osi_chmod(const char* path, int mode, ptr callback) {
  uv_fs_t* req = malloc_container(uv_fs_t);
  if (!req)
    return osi_make_error_pair("osi_chmod", UV_ENOMEM);
  Slock_object(callback);
  req->data = callback;
  int rc = uv_fs_chmod(osi_loop, req, path, mode, return_fs_result_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return osi_make_error_pair("uv_fs_chmod", rc);
  }
  return Strue;
}

void osi_close_path_watcher(uptr watcher) {
  uv_fs_event_stop((uv_fs_event_t*)watcher);
  uv_close((uv_handle_t*)watcher, close_handle_data_cb);
}

ptr osi_close_port(uptr port, ptr callback) {
  return (*(osi_port_vtable_t**)port)->close(port, callback);
}

void osi_close_tcp_listener(uptr listener) {
  uv_close((uv_handle_t*)listener, close_handle_data_cb);
}

ptr osi_connect_tcp(const char* node, const char* service, ptr callback) {
  tcp_connect_t* p = malloc_container(tcp_connect_t);
  if (!p)
    return osi_make_error_pair("osi_connect_tcp", UV_ENOMEM);
  stream_port_t* port = p->stream_port = malloc_container(stream_port_t);
  if (!port) {
    free(p);
    return osi_make_error_pair("osi_connect_tcp", UV_ENOMEM);
  }
  int rc = uv_tcp_init(osi_loop, &(port->h.tcp));
  if (rc < 0) {
    free(port);
    free(p);
    return osi_make_error_pair("uv_tcp_init", rc);
  }
  port->vtable = &tcp_vtable;
  port->close_callback = 0;
  port->read_callback = 0;
  port->write_callback = 0;
  Slock_object(callback);
  p->callback = callback;
  struct addrinfo hints;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_UNSPEC;
  hints.ai_protocol = IPPROTO_TCP;
  hints.ai_socktype = SOCK_STREAM;
  rc = uv_getaddrinfo(osi_loop, &(p->getaddrinfo), connect_tcp_addrinfo_cb, node, service, &hints);
  if (rc < 0) {
    Sunlock_object(callback);
    uv_close(&(port->h.handle), close_stream_cb);
    free(p);
    return osi_make_error_pair("uv_getaddrinfo", rc);
  }
  return Strue;
}

ptr osi_make_directory(const char* path, int mode, ptr callback) {
  uv_fs_t* req = malloc_container(uv_fs_t);
  if (!req)
    return osi_make_error_pair("osi_make_directory", UV_ENOMEM);
  Slock_object(callback);
  req->data = callback;
  int rc = uv_fs_mkdir(osi_loop, req, path, mode, return_fs_result_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return osi_make_error_pair("uv_fs_mkdir", rc);
  }
  return Strue;
}

ptr osi_make_uuid(void) {
#ifdef _WIN32
  ptr r = Smake_bytevector(sizeof(UUID), 0);
  UuidCreate((UUID*)Sbytevector_data(r));
#else
  ptr r = Smake_bytevector(sizeof(uuid_t), 0);
  uuid_generate(*(uuid_t*)Sbytevector_data(r));
#endif
  return r;
}

static uv_timer_t g_timer;

void osi_exit(int status) {
  uv_cond_destroy(&g_send_request.cond);
  uv_mutex_destroy(&g_send_request.mutex);
  uv_timer_stop(&g_timer);
  uv_close((uv_handle_t*)&g_timer, NULL);
  uv_close((uv_handle_t*)&g_send_request.async, NULL);
  osi_get_callbacks(0); // drop any callbacks since we're exiting
  // force _exit if loop is still busy, since exit() may block
  g_exit.force = uv_loop_close(osi_loop);
  if (g_exit.initialized) {
    g_exit.status = status;
    longjmp(g_exit.buf, -1);
  }
  _exit(status);
}

ptr osi_spawn(const char* path, ptr args, ptr callback) {
  int argc = string_list_length(args);
  if (argc < 0)
    return osi_make_error_pair("osi_spawn", UV_EINVAL);
  // Child stdin
  stream_port_t* in_port = malloc_container(stream_port_t);
  if (!in_port)
    return osi_make_error_pair("osi_spawn", UV_ENOMEM);
  in_port->vtable = &pipe_vtable;
  in_port->close_callback = 0;
  in_port->read_callback = 0;
  in_port->write_callback = 0;
  int rc = uv_pipe_init(osi_loop, &(in_port->h.pipe), 0);
  if (rc < 0) {
    free(in_port);
    return osi_make_error_pair("uv_pipe_init", rc);
  }
  // Child stdout
  stream_port_t* out_port = malloc_container(stream_port_t);
  if (!out_port) {
    uv_close(&(in_port->h.handle), close_stream_cb);
    return osi_make_error_pair("osi_spawn", UV_ENOMEM);
  }
  out_port->vtable = &pipe_vtable;
  out_port->close_callback = 0;
  out_port->read_callback = 0;
  out_port->write_callback = 0;
  rc = uv_pipe_init(osi_loop, &(out_port->h.pipe), 0);
  if (rc < 0) {
    uv_close(&(in_port->h.handle), close_stream_cb);
    free(out_port);
    return osi_make_error_pair("uv_pipe_init", rc);
  }
  // Child stderr
  stream_port_t* err_port = malloc_container(stream_port_t);
  if (!err_port) {
    uv_close(&(in_port->h.handle), close_stream_cb);
    uv_close(&(out_port->h.handle), close_stream_cb);
    return osi_make_error_pair("osi_spawn", UV_ENOMEM);
  }
  err_port->vtable = &pipe_vtable;
  err_port->close_callback = 0;
  err_port->read_callback = 0;
  err_port->write_callback = 0;
  rc = uv_pipe_init(osi_loop, &(err_port->h.pipe), 0);
  if (rc < 0) {
    uv_close(&(in_port->h.handle), close_stream_cb);
    uv_close(&(out_port->h.handle), close_stream_cb);
    free(err_port);
    return osi_make_error_pair("uv_pipe_init", rc);
  }
  // Build argument list
  char** argv = (char**)malloc((argc + 2) * sizeof(char*));
  if (!argv) {
    uv_close(&(in_port->h.handle), close_stream_cb);
    uv_close(&(out_port->h.handle), close_stream_cb);
    uv_close(&(err_port->h.handle), close_stream_cb);
    return osi_make_error_pair("osi_spawn", UV_ENOMEM);
  }
  {
    char** arg = argv;
    *arg++ = (char*)path;
    ptr x = args;
    while (Spairp(x)) {
      size_t l;
      if (!(*arg++ = osi_string_to_utf8(Scar(x), &l)))
        break;
      x = Scdr(x);
    }
    *arg = NULL;
  }
  // Spawn the process
  uv_process_t* p = malloc_container(uv_process_t);
  if (!p) {
    uv_close(&(in_port->h.handle), close_stream_cb);
    uv_close(&(out_port->h.handle), close_stream_cb);
    uv_close(&(err_port->h.handle), close_stream_cb);
    free_argv(argv);
    return osi_make_error_pair("osi_spawn", UV_ENOMEM);
  }
  uv_stdio_container_t stdio[3];
  stdio[0].flags = UV_CREATE_PIPE | UV_READABLE_PIPE;
  stdio[0].data.stream = &(in_port->h.stream);
  stdio[1].flags = UV_CREATE_PIPE | UV_WRITABLE_PIPE;
  stdio[1].data.stream = &(out_port->h.stream);
  stdio[2].flags = UV_CREATE_PIPE | UV_WRITABLE_PIPE;
  stdio[2].data.stream = &(err_port->h.stream);
  uv_process_options_t options = {
    .exit_cb = process_exit_cb,
    .file = path,
    .args = argv,
    .env = NULL,
    .cwd = NULL,
    .flags = 0,
    .stdio_count = 3,
    .stdio = stdio,
    .uid = 0,
    .gid = 0
  };
  Slock_object(callback);
  p->data = callback;
  rc = uv_spawn(osi_loop, p, &options);
  free_argv(argv);
  if (rc < 0) {
    uv_close(&(in_port->h.handle), close_stream_cb);
    uv_close(&(out_port->h.handle), close_stream_cb);
    uv_close(&(err_port->h.handle), close_stream_cb);
    uv_close((uv_handle_t*)p, close_handle_data_cb);
    return osi_make_error_pair("uv_spawn", rc);
  }
  ptr r = Smake_vector(4, 0);
  Svector_set(r, 0, Sunsigned((uptr)in_port));
  Svector_set(r, 1, Sunsigned((uptr)out_port));
  Svector_set(r, 2, Sunsigned((uptr)err_port));
  Svector_set(r, 3, Sinteger32(p->pid));
  return r;
}

ptr osi_spawn_detached(const char* path, ptr args) {
  int argc = string_list_length(args);
  if (argc < 0)
    return osi_make_error_pair("osi_spawn_detached", UV_EINVAL);
  // Build argument list
  char** argv = (char**)malloc((argc + 2) * sizeof(char*));
  if (!argv)
    return osi_make_error_pair("osi_spawn_detached", UV_ENOMEM);
  {
    char** arg = argv;
    *arg++ = (char*)path;
    ptr x = args;
    while (Spairp(x)) {
      size_t l;
      if (!(*arg++ = osi_string_to_utf8(Scar(x), &l)))
        break;
      x = Scdr(x);
    }
    *arg = NULL;
  }
  // Spawn the process
  uv_process_t* p = malloc_container(uv_process_t);
  if (!p) {
    free_argv(argv);
    return osi_make_error_pair("osi_spawn_detached", UV_ENOMEM);
  }
  uv_process_options_t options = {
    .exit_cb = process_exit_cb,
    .file = path,
    .args = argv,
    .env = NULL,
    .cwd = NULL,
    .flags = UV_PROCESS_DETACHED,
    .stdio_count = 0,
    .stdio = NULL,
    .uid = 0,
    .gid = 0
  };
  p->data = 0;
  int rc = uv_spawn(osi_loop, p, &options);
  free_argv(argv);
  if (rc < 0) {
    uv_close((uv_handle_t*)p, close_handle_data_cb);
    return osi_make_error_pair("uv_spawn", rc);
  }
  return Sinteger32(p->pid);
}

ptr osi_kill(int pid, int signum) {
  int rc = uv_kill(pid, signum);
  if (rc < 0)
    return osi_make_error_pair("uv_kill", rc);
  return Strue;
}

int osi_get_pid() {
  return uv_os_getpid();
}

static void fire_signal(uv_signal_t* handle, int signum) {
  (void)handle;
  ptr callback = Stop_level_value(Sstring_to_symbol("@deliver-signal"));
  osi_add_callback1(callback, Sinteger(signum));
}

static void close_signal_cb(uv_handle_t* handle) {
  free(handle);
}

ptr osi_start_signal(int signum) {
  uv_signal_t* signal = malloc_container(uv_signal_t);
  if (!signal)
    return osi_make_error_pair("osi_start_signal", UV_ENOMEM);
  int rc = uv_signal_init(osi_loop, signal);
  if (rc) {
    free(signal);
    return osi_make_error_pair("uv_signal_init", rc);
  }
  rc = uv_signal_start(signal, fire_signal, signum);
  if (rc) {
    uv_close((uv_handle_t*)signal, close_signal_cb);
    return osi_make_error_pair("uv_signal_start", rc);
  }
  return Sunsigned((uptr)signal);
}

ptr osi_stop_signal(uptr handler) {
  uv_signal_t* signal = (uv_signal_t*)handler;
  int rc = uv_signal_stop(signal);
  if (rc)
    return osi_make_error_pair("uv_signal_stop", rc);
  uv_close((uv_handle_t*)signal, close_signal_cb);
  return Strue;
}

ptr osi_get_file_size(uptr port, ptr callback) {
  fs_port_t* p = (fs_port_t*)port;
  if (p->vtable != &file_vtable)
    return osi_make_error_pair("osi_get_file_size", UV_EINVAL);
  uv_fs_t* req = malloc_container(uv_fs_t);
  if (!req)
    return osi_make_error_pair("osi_get_file_size", UV_ENOMEM);
  Slock_object(callback);
  req->data = callback;
  int rc = uv_fs_fstat(osi_loop, req, p->file, get_file_size_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return osi_make_error_pair("uv_fs_fstat", rc);
  }
  return Strue;
}

ptr osi_get_executable_path(void) {
  char buf[32768];
  size_t n = sizeof(buf);
  int rc = uv_exepath(buf, &n);
  if (rc < 0)
    return osi_make_error_pair("uv_exepath", rc);
  return Sstring_utf8(buf, n);
}

ptr osi_get_real_path(const char* path, ptr callback) {
  uv_fs_t* req = malloc_container(uv_fs_t);
  if (!req)
    return osi_make_error_pair("osi_get_real_path", UV_ENOMEM);
  Slock_object(callback);
  req->data = callback;
  int rc = uv_fs_realpath(osi_loop, req, path, get_real_path_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return osi_make_error_pair("uv_fs_realpath", rc);
  }
  return Strue;
}

ptr osi_get_ip_address(uptr port) {
  stream_port_t* p = (stream_port_t*)port;
  if (p->vtable != &tcp_vtable)
    return osi_make_error_pair("osi_get_ip_address", UV_EINVAL);
  struct sockaddr_storage addr;
  int addr_len = sizeof(addr);
  int rc = uv_tcp_getpeername(&(p->h.tcp), (struct sockaddr*)&addr, &addr_len);
  if (rc < 0)
    return osi_make_error_pair("uv_tcp_getpeername", rc);
  char name[256];
  if (addr.ss_family == AF_INET) {
    rc = uv_ip4_name((struct sockaddr_in*)&addr, name, sizeof(name) - 6);
    if (rc < 0)
      return osi_make_error_pair("uv_ip4_name", rc);
    size_t len = strlen(name);
    snprintf(name + len, sizeof(name) - len, ":%d", ntohs(((struct sockaddr_in*)&addr)->sin_port));
  } else if (addr.ss_family == AF_INET6) {
    name[0] = '[';
    rc = uv_ip6_name((struct sockaddr_in6*)&addr, name + 1, sizeof(name) - 8);
    if (rc < 0)
      return osi_make_error_pair("uv_ip6_name", rc);
    size_t len = strlen(name);
    snprintf(name + len, sizeof(name) - len, "]:%d", ntohs(((struct sockaddr_in6*)&addr)->sin6_port));
  } else
    return osi_make_error_pair("osi_get_ip_address", UV_EAI_FAMILY);
  return Sstring_utf8(name, -1);
}

ptr osi_get_tcp_listener_port(uptr listener) {
  struct sockaddr_storage addr;
  int addr_len = sizeof(addr);
  int rc = uv_tcp_getsockname((uv_tcp_t*)listener, (struct sockaddr*)&addr, &addr_len);
  if (rc < 0)
    return osi_make_error_pair("uv_tcp_getsockname", rc);
  if (addr.ss_family == AF_INET)
    return Sfixnum(ntohs(((struct sockaddr_in*)&addr)->sin_port));
  if (addr.ss_family == AF_INET6)
    return Sfixnum(ntohs(((struct sockaddr_in6*)&addr)->sin6_port));
  else
    return osi_make_error_pair("osi_get_tcp_listener_port", UV_EAI_FAMILY);
}

ptr osi_get_hostname(void) {
  static char buf[1024];
  size_t size = sizeof(buf);
  int rc = uv_os_gethostname(buf, &size);
  if (0 == rc)
    return Sstring_utf8(buf, size);
  else
    return osi_make_error_pair("uv_os_gethostname", rc);
}

uint64_t osi_get_hrtime(void) {
  return uv_hrtime();
}

ptr osi_get_uname(void) {
  uv_utsname_t buf;
  int rc = uv_os_uname(&buf);
  if (rc)
    return osi_make_error_pair("uv_os_uname", rc);
  ptr v = Smake_vector(5, 0);
  Svector_set(v, 0, Sstring_to_symbol("<uname>"));
  Svector_set(v, 1, Sstring_utf8(buf.sysname, -1));
  Svector_set(v, 2, Sstring_utf8(buf.release, -1));
  Svector_set(v, 3, Sstring_utf8(buf.version, -1));
  Svector_set(v, 4, Sstring_utf8(buf.machine, -1));
  return v;
}

int osi_is_quantum_over(void) {
  return (uv_hrtime() >= g_threshold) ? 1 : 0;
}

ptr osi_list_directory(const char* path, ptr callback) {
  uv_fs_t* req = malloc_container(uv_fs_t);
  if (!req)
    return osi_make_error_pair("osi_list_directory", UV_ENOMEM);
  Slock_object(callback);
  req->data = callback;
  int rc = uv_fs_scandir(osi_loop, req, path, 0, list_directory_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return osi_make_error_pair("uv_fs_scandir", rc);
  }
  return Strue;
}

ptr osi_listen_tcp(const char* address, uint16_t port, ptr callback) {
  struct sockaddr_storage addr;
  const char* who;
  int rc;
  if (strchr(address, '.')) {
    who = "uv_ip4_addr";
    rc = uv_ip4_addr(address, port, (struct sockaddr_in*)&addr);
  } else {
    who = "uv_ip6_addr";
    rc = uv_ip6_addr(address, port, (struct sockaddr_in6*)&addr);
  }
  if (rc < 0)
    return osi_make_error_pair(who, rc);
  uv_tcp_t* listener = malloc_container(uv_tcp_t);
  if (!listener)
    return osi_make_error_pair("osi_listen_tcp", UV_ENOMEM);
  listener->data = 0;
  rc = uv_tcp_init(osi_loop, listener);
  if (rc < 0) {
    free(listener);
    return osi_make_error_pair("uv_tcp_init", rc);
  }
  rc = uv_tcp_bind(listener, (struct sockaddr*)&addr, 0);
  if (rc < 0) {
    uv_close((uv_handle_t*)listener, close_handle_data_cb);
    return osi_make_error_pair("uv_tcp_bind", rc);
  }
  Slock_object(callback);
  listener->data = callback;
  rc = uv_listen((uv_stream_t*)listener, SOMAXCONN, listen_tcp_cb);
  if (rc < 0) {
    uv_close((uv_handle_t*)listener, close_handle_data_cb);
    return osi_make_error_pair("uv_listen", rc);
  }
  return Sunsigned((uptr)listener);
}

uint64_t osi_get_time(void) {
#ifdef _WIN32
  uint64_t now;
  GetSystemTimeAsFileTime((LPFILETIME)&now);
  // now is the number of 100-nanosecond intervals since 1 Jan 1601 (UTC).
  // 11644473600000 is the number of milliseconds from 1 Jan 1601 to 1 Jan 1970.
  return (now / 10000) - 11644473600000;
#else
  struct timespec now;
  if (clock_gettime(CLOCK_REALTIME, &now))
    return 0;
  return now.tv_sec * (uint64_t)1000 + (now.tv_nsec / 1000000);
#endif
}

ptr osi_list_uv_handles(void) {
  ptr ls = Snil;
  uv_walk(osi_loop, list_uv_cb, &ls);
  return ls;
}

ptr osi_open_fd(int fd, int close) {
  if ((fd < 0) || (close && (fd <= 2)))
    return osi_make_error_pair("osi_open_fd", UV_EINVAL);
  fs_port_t* port = malloc_container(fs_port_t);
  if (!port)
    return osi_make_error_pair("osi_open_fd", UV_ENOMEM);
  port->vtable = close ? &file_vtable : &fd_vtable;
  port->file = (uv_file)fd;
  return Sunsigned((uptr)port);
}

ptr osi_open_file(const char* path, int flags, int mode, ptr callback) {
  uv_fs_t* req = malloc_container(uv_fs_t);
  if (!req)
    return osi_make_error_pair("osi_open_file", UV_ENOMEM);
  Slock_object(callback);
  req->data = callback;
  int rc = uv_fs_open(osi_loop, req, path, flags, mode, open_fs_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return osi_make_error_pair("uv_fs_open", rc);
  }
  return Strue;
}

ptr osi_remove_directory(const char* path, ptr callback) {
  uv_fs_t* req = malloc_container(uv_fs_t);
  if (!req)
    return osi_make_error_pair("osi_remove_directory", UV_ENOMEM);
  Slock_object(callback);
  req->data = callback;
  int rc = uv_fs_rmdir(osi_loop, req, path, return_fs_result_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return osi_make_error_pair("uv_fs_rmdir", rc);
  }
  return Strue;
}

ptr osi_rename(const char* path, const char* new_path, ptr callback) {
  uv_fs_t* req = malloc_container(uv_fs_t);
  if (!req)
    return osi_make_error_pair("osi_rename", UV_ENOMEM);
  Slock_object(callback);
  req->data = callback;
  int rc = uv_fs_rename(osi_loop, req, path, new_path, return_fs_result_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return osi_make_error_pair("uv_fs_rename", rc);
  }
  return Strue;
}

ptr osi_get_home_directory(void) {
  static char buffer[32768];
  size_t len = sizeof(buffer);
  int rc = uv_os_homedir(buffer, &len);
  if (rc < 0)
    return osi_make_error_pair("uv_os_homedir", rc);
  return Sstring_utf8(buffer, len);
}

ptr osi_get_temp_directory(void) {
  static char buffer[32768];
  size_t len = sizeof(buffer);
  int rc = uv_os_tmpdir(buffer, &len);
  if (rc < 0)
    return osi_make_error_pair("uv_os_tmpdir", rc);
  return Sstring_utf8(buffer, len);
}

const char* osi_get_error_text(int err) {
  static char buf[32];
  switch (err) {
#define UV_STRERROR_GEN(name, msg) case UV_ ## name: return msg;
    UV_ERRNO_MAP(UV_STRERROR_GEN)
#define SQLITE_STRERROR_GEN(name) case TRANSLATE_SQLITE_ERRNO(name): return #name;
    SQLITE_ERRNO_MAP(SQLITE_STRERROR_GEN)
  }
  snprintf(buf, sizeof(buf), "Error code %d", err);
  return buf;
}

ptr osi_read_port(uptr port, ptr buffer, size_t start_index, uint32_t size, int64_t offset, ptr callback) {
  size_t last = start_index + size;
  if (!Sbytevectorp(buffer) ||
      (last <= start_index) || // size is 0 or start_index + size overflowed
      (last > (size_t)Sbytevector_length(buffer)))
    return osi_make_error_pair("osi_read_port", UV_EINVAL);
  return (*(osi_port_vtable_t**)port)->read(port, buffer, start_index, size, offset, callback);
}

static void get_callbacks_timer_cb(uv_timer_t* handle) {
  handle->data = handle;
  uv_stop(handle->loop);
}

ptr osi_get_callbacks(uint64_t timeout) {
  g_callbacks = Snil;
  uv_update_time(osi_loop);
  if (0 == timeout)
    uv_run(osi_loop, UV_RUN_NOWAIT);
  else {
    g_timer.data = 0;
    uv_timer_start(&g_timer, get_callbacks_timer_cb, timeout, 0);
    uv_run(osi_loop, UV_RUN_ONCE);
    if (0 == g_timer.data) { // timer didn't fire
      uv_timer_stop(&g_timer);
      uv_run(osi_loop, UV_RUN_NOWAIT);
    }
  }
  ptr callbacks = g_callbacks;
  g_callbacks = 0;
  return callbacks;
}

void osi_set_quantum(uint64_t nanoseconds) {
  g_threshold = uv_hrtime() + nanoseconds;
}

ptr osi_get_stat(const char* path, int follow, ptr callback) {
  uv_fs_t* req = malloc_container(uv_fs_t);
  if (!req)
    return osi_make_error_pair("osi_get_stat", UV_ENOMEM);
  Slock_object(callback);
  req->data = callback;
  int rc = follow ?
           uv_fs_stat(osi_loop, req, path, stat_cb) :
           uv_fs_lstat(osi_loop, req, path, stat_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return osi_make_error_pair(follow ? "uv_fs_stat" : "uv_fs_lstat", rc);
  }
  return Strue;
}

ptr osi_unlink(const char* path, ptr callback) {
  uv_fs_t* req = malloc_container(uv_fs_t);
  if (!req)
    return osi_make_error_pair("osi_unlink", UV_ENOMEM);
  Slock_object(callback);
  req->data = callback;
  int rc = uv_fs_unlink(osi_loop, req, path, return_fs_result_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return osi_make_error_pair("uv_fs_unlink", rc);
  }
  return Strue;
}

ptr osi_watch_path(const char* path, ptr callback) {
  uv_fs_event_t* watcher = malloc_container(uv_fs_event_t);
  if (!watcher)
    return osi_make_error_pair("osi_watch_path", UV_ENOMEM);
  watcher->data = 0;
  int rc = uv_fs_event_init(osi_loop, watcher);
  if (rc < 0) {
    free(watcher);
    return osi_make_error_pair("uv_fs_event_init", rc);
  }
  Slock_object(callback);
  watcher->data = callback;
  rc = uv_fs_event_start(watcher, watch_path_cb, path, 0);
  if (rc < 0) {
    uv_close((uv_handle_t*)watcher, close_handle_data_cb);
    return osi_make_error_pair("uv_fs_event_start", rc);
  }
  return Sunsigned((uptr)watcher);
}

ptr osi_write_port(uptr port, ptr buffer, size_t start_index, uint32_t size, int64_t offset, ptr callback) {
  size_t last = start_index + size;
  if (!Sbytevectorp(buffer) ||
      (last <= start_index) || // size is 0 or start_index + size overflowed
      (last > (size_t)Sbytevector_length(buffer)))
    return osi_make_error_pair("osi_write_port", UV_EINVAL);
  return (*(osi_port_vtable_t**)port)->write(port, buffer, start_index, size, offset, callback);
}

ptr osi_open_SHA1() {
  SHA1Context* ctxt = malloc_container(SHA1Context);
  if (!ctxt)
    return osi_make_error_pair(__func__, UV_ENOMEM);
  int r = SHA1Reset(ctxt);
  if (r != shaSuccess) {
    free(ctxt);
    return osi_make_error_pair("SHA1Reset", r);
  }
  return Sunsigned64((uptr)ctxt);
}

ptr osi_hash_data(SHA1Context* ctxt, ptr bv, size_t start_index, uint32_t size) {
  size_t last = start_index + size;
  if (!Sbytevectorp(bv) ||
      (last < start_index) || // size < 0 or start_index + size overflowed
      (last > (size_t)(Sbytevector_length(bv))))
    return osi_make_error_pair(__func__, UV_EINVAL);
  if (!size)
    return Strue;
  int r = SHA1Input(ctxt, (const uint8_t*)&Sbytevector_u8_ref(bv, start_index), size);
  if (r != shaSuccess)
    return osi_make_error_pair("SHA1Input", r);
  return Strue;
}

ptr osi_get_SHA1(SHA1Context* ctxt) {
  ptr bv = Smake_bytevector(SHA1HashSize, 0);
  int r = SHA1Result(ctxt, Sbytevector_data(bv));
  if (r != shaSuccess)
    return osi_make_error_pair("SHA1Result", r);
  SHA1Reset(ctxt); // defer error check until next use
  return bv;
}

void osi_close_SHA1(SHA1Context* ctxt) {
  free(ctxt);
}

static void send_request_async_cb(uv_async_t* handle) {
  (void)handle;
  uv_mutex_lock(&g_send_request.mutex);
  g_send_request.code(g_send_request.payload);
  uv_cond_t* sender = g_send_request.sender;
  g_send_request.code = NULL;
  g_send_request.payload = NULL;
  g_send_request.sender = NULL;
  uv_cond_signal(sender);
  uv_mutex_unlock(&g_send_request.mutex);
}

static osi_thread_t thread_self() {
#ifdef _WIN32
  return GetCurrentThreadId();
#else
  return pthread_self();
#endif
}

int osi_send_request(handle_request_func code, void* payload) {
  if (!code) return UV_EINVAL;
  if (thread_self() == g_scheme_thread) return UV_EPERM;
  uv_mutex_lock(&g_send_request.mutex);
  // wait our turn, guarding against spurious wakeup
  while (g_send_request.sender) {
    uv_cond_wait(&g_send_request.cond, &g_send_request.mutex);
  }
  int rc = uv_async_send(&g_send_request.async);
  if (rc) {
    uv_mutex_unlock(&g_send_request.mutex);
    return rc;
  }
  // add our work if async send succeeded
  g_send_request.code = code;
  g_send_request.payload = payload;
  uv_cond_t self;
  uv_cond_init(&self);
  g_send_request.sender = &self;
  // wait for async callback to consume work
  while (&self == g_send_request.sender) {
    uv_cond_wait(&self, &g_send_request.mutex);
  }
  uv_cond_destroy(&self);
  // let the next thread in
  uv_cond_signal(&g_send_request.cond);
  uv_mutex_unlock(&g_send_request.mutex);
  return 0;
}

int osi_is_service() {
  return g_is_service;
}

// Cannot use exports of scheme.h here; the Scheme heap may not have
// been initialized.
void osi_init(void) {
  if (osi_loop)
    return;
  uv_disable_stdio_inheritance();
  static uv_loop_t g_loop;
  uv_loop_init(&g_loop);
  osi_loop = &g_loop;
  uv_timer_init(osi_loop, &g_timer);
  g_callbacks = 0;
  g_scheme_thread = thread_self();
  uv_async_init(osi_loop, &g_send_request.async, send_request_async_cb);
  uv_mutex_init(&g_send_request.mutex);
  uv_cond_init(&g_send_request.cond);
#ifdef _WIN32
  timeBeginPeriod(1); // Set timer resolution to 1 ms.
#endif
}
