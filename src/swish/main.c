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

#ifndef _WIN32
// Unix-like systems

int swish_run(int argc, const char* argv[], void (*custom_init)(void));

int main(int argc, const char* argv[]) {
  return swish_run(argc, argv, 0);
}
#else
// Windows

#define _CRT_SECURE_NO_WARNINGS
#include <wchar.h>
#include <windows.h>

typedef int (*swish_run_func)(int argc, const char* argv[], void (*custom_init)(void));

static char* to_utf8(wchar_t* arg) {
  int len = WideCharToMultiByte(CP_UTF8, 0, arg, -1, NULL, 0, NULL, NULL);
  if (0 == len) {
    fwprintf(stderr, L"Invalid argument: %s\n", arg);
    exit(1);
  }
  char* arg8 = (char*)malloc(len * sizeof(char));
  WideCharToMultiByte(CP_UTF8, 0, arg, -1, arg8, len, NULL, NULL);
  return arg8;
}

static HMODULE load_library(const wchar_t* name) {
  HMODULE lib = LoadLibraryW(name);
  if (lib == NULL) {
    fwprintf(stderr, L"%s is not installed.\n", name);
    exit(1);
  }
  return lib;
}

#define DLL(x) L#x ".dll"
#define XDLL(x) DLL(x)

#define reserve(a,b,c) max(_countof(a), max(_countof(b), _countof(c)))

int wmain(int argc, wchar_t* argv[], wchar_t* envp[]) {
  // Convert arguments to UTF-8
  char** argv8 = (char**)malloc((argc + 1) * sizeof(char*));
  for (int i = 0; i < argc; i++) {
    argv8[i] = to_utf8(argv[i]);
  }
  argv8[argc] = NULL;

  // Find application directory
  wchar_t name[32768];
  DWORD max_len = _countof(name) - reserve(DLL(libuv), DLL(sqlite3), DLL(osi));
  DWORD name_len = GetModuleFileNameW(NULL, name, _countof(name));
  if ((name_len == 0) || (name_len > max_len)) {
    fwprintf(stderr, L"GetModuleFileName failed\n");
    exit(2);
  }
  wchar_t* filepart = &name[name_len];
  while ((filepart > name) && (*(filepart - 1) != '\\')) filepart--;

  // Disable error messages during LoadLibrary.
  SetErrorMode(SEM_FAILCRITICALERRORS);

  // Load Chez Scheme's DLL using standard search order
  load_library(XDLL(SCHEME_LIB));

  // Load Swish DLLs by absolute path
  wcscpy(filepart, DLL(libuv));
  load_library(name);
  wcscpy(filepart, DLL(sqlite3));
  load_library(name);
  wcscpy(filepart, DLL(osi));
  HMODULE osi_lib = load_library(name);

  swish_run_func swish_run = (swish_run_func)GetProcAddress(osi_lib, "swish_run");
  if (swish_run == NULL) {
    fwprintf(stderr, L"Failed to find the swish_run entry point\n");
    exit(3);
  }
  return swish_run(argc, argv8, 0);
}
#endif
