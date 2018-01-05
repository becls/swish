// Copyright 2017 Beckman Coulter, Inc.
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

#ifdef _WIN32
#pragma comment(lib, "advapi32.lib")
#pragma comment(lib, "iphlpapi.lib")
#pragma comment(lib, "psapi.lib")
#pragma comment(lib, "rpcrt4.lib")
#pragma comment(lib, "user32.lib")
#pragma comment(lib, "userenv.lib")
#pragma comment(lib, "winmm.lib")
#pragma comment(lib, "ws2_32.lib")
#endif

typedef struct {
  ptr (*close)(uptr port, ptr callback);
  ptr (*read)(uptr port, ptr buffer, size_t start_index, uint32_t size, int64_t offset, ptr callback);
  ptr (*write)(uptr port, ptr buffer, size_t start_index, uint32_t size, int64_t offset, ptr callback);
} port_vtable_t;

typedef struct {
  port_vtable_t* vtable;
  uv_file file;
} fs_port_t;

typedef struct {
  port_vtable_t* vtable;
  ptr close_callback;
  ptr read_buffer;
  size_t read_start_index;
  uint32_t read_size;
  ptr read_callback;
  uv_write_t write_req;
  ptr write_buffer;
  uint32_t write_size;
  ptr write_callback;
  uv_tcp_t tcp;
} tcp_port_t;

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
  tcp_port_t* tcp_port;
  uv_connect_t connect;
} tcp_connect_t;

static uint64_t g_tick;
static ptr g_callbacks;

void add_callback1(ptr callback, ptr arg) {
  g_callbacks = Scons(Scons(callback, Scons(arg, Snil)), g_callbacks);
}

void add_callback2(ptr callback, ptr arg1, ptr arg2) {
  g_callbacks = Scons(Scons(callback, Scons(arg1, Scons(arg2, Snil))), g_callbacks);
}

ptr make_error_pair(const char* who, int error)
{
  return Scons(Sstring_to_symbol(who), Sinteger(error));
}

static ptr close_port_nosys(uptr port, ptr callback) {
  (void)port;
  (void)callback;
  return make_error_pair("close_port", UV_ENOSYS);
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
  add_callback1(callback, Sinteger(result));
}

static ptr read_fs_port(uptr port, ptr buffer, size_t start_index, uint32_t size, int64_t offset, ptr callback) {
  fs_port_t* p = (fs_port_t*)port;
  rw_fs_req_t* req = malloc_container(rw_fs_req_t);
  if (!req)
    return make_error_pair("osi_read_port", UV_ENOMEM);
  Slock_object(buffer);
  Slock_object(callback);
  req->buffer = buffer;
  req->callback = callback;
  uv_buf_t buf = {
    .base = (char*)&Sbytevector_u8_ref(buffer, start_index),
    .len = size};
  int rc = uv_fs_read(g_loop, &req->fs, p->file, &buf, 1, offset, rw_fs_cb);
  if (rc < 0) {
    Sunlock_object(buffer);
    Sunlock_object(callback);
    free(req);
    return make_error_pair("uv_fs_read", rc);
  }
  return Strue;
}

static ptr write_fs_port(uptr port, ptr buffer, size_t start_index, uint32_t size, int64_t offset, ptr callback) {
  fs_port_t* p = (fs_port_t*)port;
  rw_fs_req_t* req = malloc_container(rw_fs_req_t);
  if (!req)
    return make_error_pair("osi_write_port", UV_ENOMEM);
  Slock_object(buffer);
  Slock_object(callback);
  req->buffer = buffer;
  req->callback = callback;
  uv_buf_t buf = {
    .base = (char*)&Sbytevector_u8_ref(buffer, start_index),
    .len = size};
  int rc = uv_fs_write(g_loop, &req->fs, p->file, &buf, 1, offset, rw_fs_cb);
  if (rc < 0) {
    Sunlock_object(buffer);
    Sunlock_object(callback);
    free(req);
    return make_error_pair("uv_fs_write", rc);
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
  add_callback1(callback, Sinteger(result));
}

static ptr close_fs_port(uptr port, ptr callback) {
  fs_port_t* p = (fs_port_t*)port;
  close_fs_req_t* req = malloc_container(close_fs_req_t);
  if (!req)
    return make_error_pair("osi_close_port", UV_ENOMEM);
  Slock_object(callback);
  req->port = p;
  req->callback = callback;
  int rc = uv_fs_close(g_loop, &req->fs, p->file, close_fs_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return make_error_pair("uv_fs_close", rc);
  }
  return Strue;
}

static port_vtable_t file_vtable = {
  .close = close_fs_port,
  .read = read_fs_port,
  .write = write_fs_port};

static void open_fs_cb(uv_fs_t* req) {
  ssize_t result = req->result;
  ptr callback = (ptr)req->data;
  uv_fs_req_cleanup(req);
  Sunlock_object(callback);
  free(req);
  if (result < 0)
    add_callback1(callback, make_error_pair("uv_fs_open", (int)result));
  else {
    fs_port_t* port = malloc_container(fs_port_t);
    port->vtable = &file_vtable;
    port->file = (uv_file)result;
    add_callback1(callback, Sunsigned((uptr)port));
  }
}

static void get_file_size_cb(uv_fs_t* req) {
  ptr callback = (ptr)req->data;
  if (req->result < 0)
    add_callback1(callback, make_error_pair("uv_fs_fstat", (int)req->result));
  else
    add_callback1(callback, Sunsigned64(req->statbuf.st_size));
  uv_fs_req_cleanup(req);
  Sunlock_object(callback);
  free(req);
}

static void return_fs_result_cb(uv_fs_t* req) {
  ptr callback = (ptr)req->data;
  add_callback1(callback, Sinteger(req->result));
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
    add_callback1(callback, Sinteger(req->result));
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
    add_callback1(callback, r);
  }
  uv_fs_req_cleanup(req);
  Sunlock_object(callback);
  free(req);
}

static void list_directory_cb(uv_fs_t* req) {
  ptr callback = (ptr)req->data;
  if (req->result < 0)
    add_callback1(callback, Sinteger(req->result));
  else {
    ptr ls = Snil;
    uv_dirent_t ent;
    while (uv_fs_scandir_next(req, &ent) >= 0)
      ls = Scons(Scons(make_scheme_string(ent.name), Sinteger(ent.type)), ls);
    add_callback1(callback, ls);
  }
  uv_fs_req_cleanup(req);
  Sunlock_object(callback);
  free(req);
}

static void read_tcp_alloc_cb(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf) {
  (void)suggested_size;
  tcp_port_t* p = container_of(handle, tcp_port_t, tcp);
  buf->base = (char*)&Sbytevector_u8_ref(p->read_buffer, p->read_start_index);
  buf->len = p->read_size;
}

static void read_tcp_cb(uv_stream_t* stream, ssize_t nread, const uv_buf_t* buf) {
  (void)buf;
  uv_read_stop(stream);
  tcp_port_t* p = container_of(stream, tcp_port_t, tcp);
  ptr callback = p->read_callback;
  Sunlock_object(p->read_buffer);
  Sunlock_object(callback);
  p->read_callback = 0;
  add_callback1(callback, Sinteger(nread));
}

static ptr read_tcp_port(uptr port, ptr buffer, size_t start_index, uint32_t size, int64_t offset, ptr callback) {
  if (-1 != offset)
    return make_error_pair("osi_read_port", UV_EINVAL);
  tcp_port_t* p = (tcp_port_t*)port;
  if (p->read_callback)
    return make_error_pair("osi_read_port", UV_EBUSY);
  Slock_object(buffer);
  Slock_object(callback);
  p->read_buffer = buffer;
  p->read_start_index = start_index;
  p->read_size = size;
  p->read_callback = callback;
  int rc = uv_read_start((uv_stream_t*)&(p->tcp), read_tcp_alloc_cb, read_tcp_cb);
  if (rc < 0) {
    Sunlock_object(buffer);
    Sunlock_object(callback);
    p->read_callback = 0;
    return make_error_pair("uv_read_start", rc);
  }
  return Strue;
}

static void write_tcp_cb(uv_write_t* req, int status) {
  tcp_port_t* p = container_of(req, tcp_port_t, write_req);
  ptr callback = p->write_callback;
  Sunlock_object(p->write_buffer);
  Sunlock_object(callback);
  p->write_callback = 0;
  add_callback1(callback, (status < 0) ? Sinteger32(status) : Sunsigned32(p->write_size));
}

static ptr write_tcp_port(uptr port, ptr buffer, size_t start_index, uint32_t size, int64_t offset, ptr callback) {
  if (-1 != offset)
    return make_error_pair("osi_write_port", UV_EINVAL);
  tcp_port_t* p = (tcp_port_t*)port;
  if (p->write_callback)
    return make_error_pair("osi_write_port", UV_EBUSY);
  Slock_object(buffer);
  Slock_object(callback);
  p->write_buffer = buffer;
  p->write_size = size;
  p->write_callback = callback;
  uv_buf_t buf = {
    .base = (char*)&Sbytevector_u8_ref(p->write_buffer, start_index),
    .len = size};
  int rc = uv_write(&(p->write_req), (uv_stream_t*)&(p->tcp), &buf, 1, write_tcp_cb);
  if (rc < 0) {
    Sunlock_object(buffer);
    Sunlock_object(callback);
    p->write_callback = 0;
    return make_error_pair("uv_write", rc);
  }
  return Strue;
}

static void close_tcp_cb(uv_handle_t* handle) {
  tcp_port_t* p = container_of(handle, tcp_port_t, tcp);
  ptr callback = p->close_callback;
  free(p);
  if (callback) {
    Sunlock_object(callback);
    add_callback1(callback, Sfixnum(0));
  }
}

static ptr close_tcp_port(uptr port, ptr callback) {
  tcp_port_t* p = (tcp_port_t*)port;
  if (p->close_callback)
    return make_error_pair("osi_close_port", UV_EBUSY);
  Slock_object(callback);
  p->close_callback = callback;
  uv_close((uv_handle_t*)&(p->tcp), close_tcp_cb);
  return Strue;
}

static port_vtable_t tcp_vtable = {
  .close = close_tcp_port,
  .read = read_tcp_port,
  .write = write_tcp_port};

static void connect_tcp_cb(uv_connect_t* req, int status) {
  tcp_connect_t* p = container_of(req, tcp_connect_t, connect);
  if (status < 0) {
    do {
      struct addrinfo* ai = p->ai;
      if (!ai) {
        ptr callback = p->callback;
        Sunlock_object(callback);
        uv_close((uv_handle_t*)&(p->tcp_port->tcp), close_tcp_cb);
        uv_freeaddrinfo(p->getaddrinfo.addrinfo);
        free(p);
        add_callback1(callback, make_error_pair("uv_tcp_connect", status));
        return;
      }
      p->ai = ai->ai_next;
      status = uv_tcp_connect(&(p->connect), &(p->tcp_port->tcp), ai->ai_addr, connect_tcp_cb);
    } while (status < 0);
    return;
  }
  ptr callback = p->callback;
  tcp_port_t* port = p->tcp_port;
  Sunlock_object(callback);
  uv_freeaddrinfo(p->getaddrinfo.addrinfo);
  free(p);
  add_callback1(callback, Sunsigned((uptr)port));
}

static void connect_tcp_addrinfo_cb(uv_getaddrinfo_t* req, int status, struct addrinfo* res) {
  tcp_connect_t* p = container_of(req, tcp_connect_t, getaddrinfo);
  if (status < 0) {
    ptr callback = p->callback;
    Sunlock_object(callback);
    uv_close((uv_handle_t*)&(p->tcp_port->tcp), close_tcp_cb);
    free(p);
    add_callback1(callback, make_error_pair("uv_getaddrinfo", status));
    return;
  }
  p->ai = res->ai_next;
  int rc = uv_tcp_connect(&(p->connect), &(p->tcp_port->tcp), res->ai_addr, connect_tcp_cb);
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
    add_callback1(callback, make_error_pair("uv_listen", status));
    return;
  }
  tcp_port_t* port = malloc_container(tcp_port_t);
  if (!port) {
    add_callback1(callback, make_error_pair("uv_listen", UV_ENOMEM));
    return;
  }
  status = uv_tcp_init(g_loop, &(port->tcp));
  if (status < 0) {
    free(port);
    add_callback1(callback, make_error_pair("uv_tcp_init", status));
    return;
  }
  port->vtable = &tcp_vtable;
  port->close_callback = 0;
  port->read_callback = 0;
  port->write_callback = 0;
  status = uv_accept(server, (uv_stream_t*)&(port->tcp));
  if (status < 0) {
    uv_close((uv_handle_t*)&(port->tcp), close_tcp_cb);
    add_callback1(callback, make_error_pair("uv_accept", status));
    return;
  }
  add_callback1(callback, Sunsigned((uptr)port));
}

static void watch_path_cb(uv_fs_event_t* handle, const char* filename, int events, int status) {
  ptr callback = (ptr)handle->data;
  if (status < 0) {
    add_callback1(callback, Sinteger(status));
    return;
  }
  add_callback2(callback, make_scheme_string(filename), Sinteger(events));
}

size_t osi_bytes_used(void) {
#if defined(__APPLE__)
  malloc_zone_pressure_relief(NULL, 0);
  struct mstats ms = mstats();
  return ms.bytes_used;
#elif defined(__linux__)
  struct mallinfo hinfo = mallinfo();
  return hinfo.hblkhd + hinfo.uordblks;
#elif defined(_WIN32)
  size_t used = 0;
  _HEAPINFO hinfo = { ._pentry = NULL };
  while (_heapwalk(&hinfo) == _HEAPOK)
    if (_USEDENTRY == hinfo._useflag)
      used += hinfo._size;
  return used;
#endif
}

ptr osi_chmod(const char* path, int mode, ptr callback) {
  uv_fs_t* req = malloc_container(uv_fs_t);
  if (!req)
    return make_error_pair("osi_chmod", UV_ENOMEM);
  Slock_object(callback);
  req->data = callback;
  int rc = uv_fs_chmod(g_loop, req, path, mode, return_fs_result_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return make_error_pair("uv_fs_chmod", rc);
  }
  return Strue;
}

void osi_close_path_watcher(uptr watcher) {
  uv_fs_event_stop((uv_fs_event_t*)watcher);
  uv_close((uv_handle_t*)watcher, close_handle_data_cb);
}

ptr osi_close_port(uptr port, ptr callback) {
  return (*(port_vtable_t**)port)->close(port, callback);
}

void osi_close_tcp_listener(uptr listener) {
  uv_close((uv_handle_t*)listener, close_handle_data_cb);
}

ptr osi_connect_tcp(const char* node, const char* service, ptr callback) {
  tcp_connect_t* p = malloc_container(tcp_connect_t);
  if (!p)
    return make_error_pair("osi_connect_tcp", UV_ENOMEM);
  tcp_port_t* port = p->tcp_port = malloc_container(tcp_port_t);
  if (!port) {
    free(p);
    return make_error_pair("osi_connect_tcp", UV_ENOMEM);
  }
  int rc = uv_tcp_init(g_loop, &(port->tcp));
  if (rc < 0) {
    free(port);
    free(p);
    return make_error_pair("uv_tcp_init", rc);
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
  rc = uv_getaddrinfo(g_loop, &(p->getaddrinfo), connect_tcp_addrinfo_cb, node, service, &hints);
  if (rc < 0) {
    Sunlock_object(callback);
    uv_close((uv_handle_t*)&(port->tcp), close_tcp_cb);
    free(p);
    return make_error_pair("uv_getaddrinfo", rc);
  }
  return Strue;
}

ptr osi_make_directory(const char* path, int mode, ptr callback) {
  uv_fs_t* req = malloc_container(uv_fs_t);
  if (!req)
    return make_error_pair("osi_make_directory", UV_ENOMEM);
  Slock_object(callback);
  req->data = callback;
  int rc = uv_fs_mkdir(g_loop, req, path, mode, return_fs_result_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return make_error_pair("uv_fs_mkdir", rc);
  }
  return Strue;
}

ptr osi_make_guid(void) {
#ifdef _WIN32
  ptr r = Smake_bytevector(sizeof(UUID), 0);
  RPC_STATUS rc = UuidCreate((UUID*)Sbytevector_data(r));
  if (RPC_S_OK != rc)
    return make_error_pair("UuidCreate", rc);
  return r;
#else
  ptr r = Smake_bytevector(sizeof(uuid_t), 0);
  uuid_generate(*(uuid_t*)Sbytevector_data(r));
  return r;
#endif
}

void osi_exit(int status) {
  _exit(status);
}

ptr osi_get_file_size(uptr port, ptr callback) {
  fs_port_t* p = (fs_port_t*)port;
  if (p->vtable != &file_vtable)
    return make_error_pair("osi_get_file_size", UV_EINVAL);
  uv_fs_t* req = malloc_container(uv_fs_t);
  if (!req)
    return make_error_pair("osi_get_file_size", UV_ENOMEM);
  Slock_object(callback);
  req->data = callback;
  int rc = uv_fs_fstat(g_loop, req, p->file, get_file_size_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return make_error_pair("uv_fs_fstat", rc);
  }
  return Strue;
}

ptr osi_get_ip_address(uptr port) {
  tcp_port_t* p = (tcp_port_t*)port;
  if (p->vtable != &tcp_vtable)
    return make_error_pair("osi_get_ip_address", UV_EINVAL);
  struct sockaddr_in6 addr;
  int addr_len = sizeof(addr);
  int rc = uv_tcp_getpeername(&(p->tcp), (struct sockaddr*)&addr, &addr_len);
  if (rc < 0)
    return make_error_pair("uv_tcp_getpeername", rc);
  char name[256];
  name[0] = '[';
  rc = uv_ip6_name(&addr, name + 1, sizeof(name) - 8);
  if (rc < 0)
    return make_error_pair("uv_ip6_name", rc);
  size_t len = strlen(name);
  snprintf(name + len, sizeof(name) - len, "]:%d", ntohs(addr.sin6_port));
  return make_scheme_string(name);
}

ptr osi_get_tcp_listener_port(uptr listener) {
  struct sockaddr_in6 addr;
  int addr_len = sizeof(addr);
  int rc = uv_tcp_getsockname((uv_tcp_t*)listener, (struct sockaddr*)&addr, &addr_len);
  if (rc < 0)
    return make_error_pair("uv_tcp_getsockname", rc);
  return Sfixnum(ntohs(addr.sin6_port));
}

ptr osi_hostname(void) {
  static char buf[1024];
  size_t size = sizeof(buf);
  int rc = uv_os_gethostname(buf, &size);
  if (0 == rc)
    return Sstring_of_length(buf, size);
  else
    return make_error_pair("uv_os_gethostname", rc);
}

uint64_t osi_hrtime(void) {
  return uv_hrtime();
}

int osi_is_tick_over(void) {
  return (uv_hrtime() >= g_tick) ? 1 : 0;
}

ptr osi_list_directory(const char* path, ptr callback) {
  uv_fs_t* req = malloc_container(uv_fs_t);
  if (!req)
    return make_error_pair("osi_list_directory", UV_ENOMEM);
  Slock_object(callback);
  req->data = callback;
  int rc = uv_fs_scandir(g_loop, req, path, 0, list_directory_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return make_error_pair("uv_fs_scandir", rc);
  }
  return Strue;
}

ptr osi_listen_tcp(uint16_t port, ptr callback) {
  uv_tcp_t* listener = malloc_container(uv_tcp_t);
  if (!listener)
    return make_error_pair("osi_listen_tcp", UV_ENOMEM);
  listener->data = 0;
  int rc = uv_tcp_init(g_loop, listener);
  if (rc < 0) {
    free(listener);
    return make_error_pair("uv_tcp_init", rc);
  }
  struct sockaddr_in6 addr;
  rc = uv_ip6_addr("::", port, &addr);
  if (rc < 0) {
    uv_close((uv_handle_t*)listener, close_handle_data_cb);
    return make_error_pair("uv_tcp_bind", rc);
  }
  rc = uv_tcp_bind(listener, (struct sockaddr*)&addr, 0);
  if (rc < 0) {
    uv_close((uv_handle_t*)listener, close_handle_data_cb);
    return make_error_pair("uv_tcp_bind", rc);
  }
  Slock_object(callback);
  listener->data = callback;
  rc = uv_listen((uv_stream_t*)listener, SOMAXCONN, listen_tcp_cb);
  if (rc < 0) {
    uv_close((uv_handle_t*)listener, close_handle_data_cb);
    return make_error_pair("uv_listen", rc);
  }
  return Sunsigned((uptr)listener);
}

uint64_t osi_now(void) {
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

ptr osi_open_file(const char* path, int flags, int mode, ptr callback) {
  uv_fs_t* req = malloc_container(uv_fs_t);
  if (!req)
    return make_error_pair("osi_open_file", UV_ENOMEM);
  Slock_object(callback);
  req->data = callback;
  int rc = uv_fs_open(g_loop, req, path, flags, mode, open_fs_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return make_error_pair("uv_fs_close", rc);
  }
  return Strue;
}

void osi_print_all_handles(void) {
  uv_print_all_handles(g_loop, stderr);
}

ptr osi_remove_directory(const char* path, ptr callback) {
  uv_fs_t* req = malloc_container(uv_fs_t);
  if (!req)
    return make_error_pair("osi_remove_directory", UV_ENOMEM);
  Slock_object(callback);
  req->data = callback;
  int rc = uv_fs_rmdir(g_loop, req, path, return_fs_result_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return make_error_pair("uv_fs_rmdir", rc);
  }
  return Strue;
}

ptr osi_rename(const char* path, const char* new_path, ptr callback) {
  uv_fs_t* req = malloc_container(uv_fs_t);
  if (!req)
    return make_error_pair("osi_rename", UV_ENOMEM);
  Slock_object(callback);
  req->data = callback;
  int rc = uv_fs_rename(g_loop, req, path, new_path, return_fs_result_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return make_error_pair("uv_fs_rename", rc);
  }
  return Strue;
}

uptr osi_stdin(void) {
  static port_vtable_t stdin_vtable = {
    .close = close_port_nosys,
    .read = read_fs_port,
    .write = write_fs_port};
  static fs_port_t stdin_port = {
    .vtable = &stdin_vtable,
    .file = 0};
  return (uptr)&stdin_port;
}

const char* osi_strerror(int err) {
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
    return make_error_pair("osi_read_port", UV_EINVAL);
  return (*(port_vtable_t**)port)->read(port, buffer, start_index, size, offset, callback);
}

static uv_timer_t g_timer;

static void get_callbacks_timer_cb(uv_timer_t* handle) {
  handle->data = handle;
  uv_stop(handle->loop);
}

ptr osi_get_callbacks(uint64_t timeout) {
  uv_update_time(g_loop);
  if (0 == timeout) {
    uv_run(g_loop, UV_RUN_NOWAIT);
  } else {
    g_timer.data = 0;
    uv_timer_start(&g_timer, get_callbacks_timer_cb, timeout, 0);
    uv_run(g_loop, UV_RUN_ONCE);
    if (0 == g_timer.data) { // timer didn't fire
      uv_timer_stop(&g_timer);
      uv_run(g_loop, UV_RUN_NOWAIT);
    }
  }
  ptr callbacks = g_callbacks;
  g_callbacks = Snil;
  return callbacks;
}

void osi_set_tick(uint64_t nanoseconds) {
  g_tick = uv_hrtime() + nanoseconds;
}

ptr osi_stat(const char* path, int follow, ptr callback) {
  uv_fs_t* req = malloc_container(uv_fs_t);
  if (!req)
    return make_error_pair("osi_stat", UV_ENOMEM);
  Slock_object(callback);
  req->data = callback;
  int rc = follow ?
    uv_fs_stat(g_loop, req, path, stat_cb) :
    uv_fs_lstat(g_loop, req, path, stat_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return make_error_pair(follow ? "uv_fs_stat" : "uv_fs_lstat", rc);
  }
  return Strue;
}

ptr osi_unlink(const char* path, ptr callback) {
  uv_fs_t* req = malloc_container(uv_fs_t);
  if (!req)
    return make_error_pair("osi_unlink", UV_ENOMEM);
  Slock_object(callback);
  req->data = callback;
  int rc = uv_fs_unlink(g_loop, req, path, return_fs_result_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req);
    return make_error_pair("uv_fs_unlink", rc);
  }
  return Strue;
}

ptr osi_watch_path(const char* path, ptr callback) {
  uv_fs_event_t* watcher = malloc_container(uv_fs_event_t);
  if (!watcher)
    return make_error_pair("osi_watch_path", UV_ENOMEM);
  watcher->data = 0;
  int rc = uv_fs_event_init(g_loop, watcher);
  if (rc < 0) {
    free(watcher);
    return make_error_pair("uv_fs_event_init", rc);
  }
  Slock_object(callback);
  watcher->data = callback;
  rc = uv_fs_event_start(watcher, watch_path_cb, path, 0);
  if (rc < 0) {
    uv_close((uv_handle_t*)watcher, close_handle_data_cb);
    return make_error_pair("uv_fs_event_start", rc);
  }
  return Sunsigned((uptr)watcher);
}

ptr osi_write_port(uptr port, ptr buffer, size_t start_index, uint32_t size, int64_t offset, ptr callback) {
  size_t last = start_index + size;
  if (!Sbytevectorp(buffer) ||
      (last <= start_index) || // size is 0 or start_index + size overflowed
      (last > (size_t)Sbytevector_length(buffer)))
    return make_error_pair("osi_write_port", UV_EINVAL);
  return (*(port_vtable_t**)port)->write(port, buffer, start_index, size, offset, callback);
}

#ifndef _WIN32
__attribute__((constructor))
#endif
static void osi_init(void) {
  g_loop = uv_default_loop();
  uv_timer_init(g_loop, &g_timer);
  g_callbacks = Snil;
#ifdef _WIN32
  timeBeginPeriod(1); // Set timer resolution to 1 ms.
#endif
}

#ifdef _WIN32
BOOL WINAPI DllMain(HINSTANCE inst, DWORD reason, LPVOID reserved) {
  if (DLL_PROCESS_ATTACH == reason)
    osi_init();
  return TRUE;
}
#endif
