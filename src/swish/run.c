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
#include <errno.h>
#include <string.h>
#include <stdlib.h>

#ifdef _WIN32 // Windows Service
#include <io.h>
#include <time.h>
#include <wchar.h>
#endif

static void (* g_aux_init)(void) = NULL;

static void swish_init(void) {
#define add_foreign(sym) Sforeign_symbol(#sym, (void *)sym)
  add_foreign(osi_bind_statement);
  add_foreign(osi_chmod);
  add_foreign(osi_clear_statement_bindings);
  add_foreign(osi_close_SHA1);
  add_foreign(osi_close_database);
  add_foreign(osi_close_path_watcher);
  add_foreign(osi_close_port);
  add_foreign(osi_close_tcp_listener);
  add_foreign(osi_connect_tcp);
  add_foreign(osi_exit);
  add_foreign(osi_finalize_statement);
  add_foreign(osi_get_SHA1);
  add_foreign(osi_get_argv);
  add_foreign(osi_get_bytes_used);
  add_foreign(osi_get_callbacks);
  add_foreign(osi_get_error_text);
  add_foreign(osi_get_executable_path);
  add_foreign(osi_get_file_size);
  add_foreign(osi_get_hostname);
  add_foreign(osi_get_hrtime);
  add_foreign(osi_get_ip_address);
  add_foreign(osi_get_last_insert_rowid);
  add_foreign(osi_get_pid);
  add_foreign(osi_get_real_path);
  add_foreign(osi_get_sqlite_status);
  add_foreign(osi_get_stat);
  add_foreign(osi_get_statement_columns);
  add_foreign(osi_get_statement_expanded_sql);
  add_foreign(osi_get_tcp_listener_port);
  add_foreign(osi_get_temp_directory);
  add_foreign(osi_get_time);
  add_foreign(osi_hash_data);
  add_foreign(osi_init);
  add_foreign(osi_interrupt_database);
  add_foreign(osi_is_quantum_over);
  add_foreign(osi_kill);
  add_foreign(osi_list_directory);
  add_foreign(osi_list_uv_handles);
  add_foreign(osi_listen_tcp);
  add_foreign(osi_make_directory);
  add_foreign(osi_make_uuid);
  add_foreign(osi_open_SHA1);
  add_foreign(osi_open_database);
  add_foreign(osi_open_fd);
  add_foreign(osi_open_file);
  add_foreign(osi_prepare_statement);
  add_foreign(osi_read_port);
  add_foreign(osi_remove_directory);
  add_foreign(osi_rename);
  add_foreign(osi_reset_statement);
  add_foreign(osi_set_quantum);
  add_foreign(osi_start_signal);
  add_foreign(osi_stop_signal);
  add_foreign(osi_spawn);
  add_foreign(osi_step_statement);
  add_foreign(osi_unlink);
  add_foreign(osi_watch_path);
  add_foreign(osi_write_port);
  if (g_aux_init) g_aux_init();
}

// We expect to find our boot file alongside the executable.
static char* get_boot_fn() {
  char execfn[32768];
  size_t n = sizeof(execfn);
  int rc = uv_exepath(execfn, &n);
  if (rc < 0) {
    perror("uv_exepath");
    exit(1);
  }

  const char* suffix = ".boot";
  size_t suffixlen = strlen(suffix);
  size_t pathlast = strlen(execfn);
  char* bootfn = (char*)malloc(sizeof(char) * (pathlast + suffixlen + 1));
  if (NULL == bootfn) {
    fprintf(stderr, "malloc failed\n");
    exit(1);
  }
#ifdef _WIN32
  const char* end = strrchr(execfn, '.');
  const char* exe = ".exe";
  if (NULL != end && _strnicmp(end, exe, 5) == 0) {
    pathlast -=  strlen(exe);
  }
#endif
  memcpy(bootfn, execfn, pathlast);
  memcpy((char*)(bootfn + pathlast), suffix, suffixlen);
  bootfn[pathlast+suffixlen] = '\0';
  return bootfn;
}

static int allow_verbose_flag(const char* bootfn) {
  size_t len = strlen(bootfn);
#if _WIN32
  const char* suffix =  "\\swish.boot";
#define COMPARE _strnicmp
#else
  const char* suffix =  "/swish.boot";
#define COMPARE strncmp
#endif
  size_t suffixlen = strlen(suffix);
  return len >= suffixlen && !COMPARE(bootfn + len - suffixlen, suffix, suffixlen);
}

static void scheme_init(int argc, const char* argv[], void (*custom_init)(void)) {
  char* bootfn = get_boot_fn();

  Sscheme_init(NULL);
  // Don't interfere with swish scripts or stand-alone swish applications that
  // want to support a --verbose option.
  if (argc >= 2 && (strcmp(argv[1], "--verbose") == 0) && allow_verbose_flag(bootfn)) {
    Sset_verbose(1);
  }
  Sregister_boot_file(bootfn);

  osi_set_argv(argc, argv);
  g_aux_init = custom_init;
  Sbuild_heap(argv[0], swish_init);
  free(bootfn);

  osi_init();
}

static int swish_start(int argc, const char* argv[]) {
  if (setjmp(g_exit.buf) == 0) {
    g_exit.initialized = 1;
    g_exit.status = Sscheme_start(argc, argv);
  }
  Sscheme_deinit();
  return g_exit.status;
}

// custom_init may be NULL or a pointer to a function that performs application-specific
// initialization during Sbuild_heap.
int swish_run(int argc, const char* argv[], void (*custom_init)(void)) {
  scheme_init(argc, argv, custom_init);
  int rc = swish_start(argc, argv);
  if (g_exit.force)
    _exit(rc);
  return rc;
}


#ifdef _WIN32
//
//
// Windows Service
//
//

SERVICE_STATUS g_service_status = {0};
SERVICE_STATUS_HANDLE g_service_status_handle = NULL;
const wchar_t* g_service_name = NULL;
int g_argc = 0;
char** g_argv = NULL;

static void console_event_handler(const char* event) {
  // This function mirrors console-event-handler in erlang.ss.
  time_t now;
  time(&now);
  struct tm now_tm;
  localtime_s(&now_tm, &now);
  char now_s[26];
  asctime_s(now_s, sizeof(now_s), &now_tm);
  fprintf(stderr, "\r\nDate: %.24s\r\n", now_s);
  fprintf(stderr, "Timestamp: %I64u\r\n", osi_get_time());
  fprintf(stderr, "Event: %s\r\n\r\n", event);
  fflush(stderr);
}

static void fatal_last_error(const char* who) {
  char msg[80];
  sprintf_s(msg, sizeof(msg), "#(fatal-error %s %u)", who, GetLastError());
  console_event_handler(msg);
  exit(4);
}

static void request_call(char* arg) {
  ptr callback = Stop_level_value(Sstring_to_symbol(arg));
  osi_add_callback_list(callback, Snil);
}

static DWORD WINAPI service_control_handler(DWORD dwControl, DWORD dwEventType, LPVOID lpEventData, LPVOID lpContext) {
  switch (dwControl) {
  case SERVICE_CONTROL_INTERROGATE:
    return NO_ERROR;
  case SERVICE_CONTROL_SHUTDOWN:
  case SERVICE_CONTROL_STOP:
    g_service_status.dwCurrentState = SERVICE_STOP_PENDING;
    g_service_status.dwWaitHint = 60000;
    SetServiceStatus(g_service_status_handle, &g_service_status);
    osi_send_request((handle_request_func)request_call, "$shutdown");
    return NO_ERROR;
  case SERVICE_CONTROL_POWEREVENT:
    if (PBT_APMSUSPEND == dwEventType)
      osi_send_request((handle_request_func)request_call, "$suspend");
    else if (PBT_APMRESUMEAUTOMATIC == dwEventType)
      osi_send_request((handle_request_func)request_call, "$resume");
    return ERROR_CALL_NOT_IMPLEMENTED;
  default:
    return ERROR_CALL_NOT_IMPLEMENTED;
  }
}

static void WINAPI service_run(DWORD _argc, wchar_t* _argv[]) {
  scheme_init(g_argc, g_argv, 0);
  g_service_status_handle = RegisterServiceCtrlHandlerExW(g_service_name, service_control_handler, NULL);
  if (NULL == g_service_status_handle)
    fatal_last_error("RegisterServiceCtrlHandlerEx");
  g_service_status.dwServiceType = SERVICE_WIN32_OWN_PROCESS;
  g_service_status.dwCurrentState = SERVICE_RUNNING;
  g_service_status.dwControlsAccepted = SERVICE_ACCEPT_STOP | SERVICE_ACCEPT_SHUTDOWN | SERVICE_ACCEPT_POWEREVENT;
  SetServiceStatus(g_service_status_handle, &g_service_status);

  int status = swish_start(g_argc, g_argv);

  char msg[80];
  sprintf_s(msg, sizeof(msg), "#(osi_exit %u)", status);
  console_event_handler(msg);
  g_service_status.dwWin32ExitCode = status;
  g_service_status.dwCurrentState = SERVICE_STOPPED;
  SetServiceStatus(g_service_status_handle, &g_service_status);
}

int swish_service(const wchar_t* service_name, const wchar_t* logfile, int argc, const char* argv[]) {
  // Redirect stdout and stderr to the specified file.
  int flog;
  if (_wsopen_s(&flog, logfile, _O_APPEND | _O_BINARY | _O_CREAT | _O_WRONLY, _SH_DENYNO, _S_IREAD | _S_IWRITE))
    return 1;

  // Scheme refers to file handles 1 and 2 directly. We simply need to
  // redirect them to the log file.  Additional linked C code may
  // refer to the stdout and stderr file pointers. Reopen them to get
  // valid handles, then redirect them to the log file. Stdin is
  // redirected to NUL.
  _dup2(flog, 1);
  _dup2(flog, 2);

  FILE* so;
  _wfreopen_s(&so, L"NUL", L"a", stdout);
  _dup2(flog, _fileno(stdout));

  FILE* se;
  _wfreopen_s(&se, L"NUL", L"a", stderr);
  _dup2(flog, _fileno(stderr));

  _close(flog);

  FILE* si;
  _wfreopen_s(&si, L"NUL", L"r", stdin);
  _dup2(_fileno(stdin), 0);

  console_event_handler("service-starting");
  g_service_name = service_name;
  g_argc = argc;
  g_argv = (char**)argv;
  SERVICE_TABLE_ENTRYW dispatchTable[] = {{(LPWSTR)service_name, service_run}, {NULL, NULL}};
  if (!StartServiceCtrlDispatcherW(dispatchTable))
    fatal_last_error("StartServiceCtrlDispatcher");
  return 0;
}
#endif // Windows Service
