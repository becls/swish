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

int swish_run(int argc, const char *argv[], void (*custom_init)(void));

#ifdef _WIN32
#include <wchar.h>
#include <windows.h>

static char* to_utf8(wchar_t* arg) {
  int len = WideCharToMultiByte(CP_UTF8, 0, arg, -1, NULL, 0, NULL, NULL);
  if (0 == len) {
    fwprintf_s(stderr, L"Invalid argument: %s\n", arg);
    exit(1);
  }
  char* arg8 = (char*)malloc(len * sizeof(char));
  WideCharToMultiByte(CP_UTF8, 0, arg, -1, arg8, len, NULL, NULL);
  return arg8;
}

int wmain(int argc, wchar_t* argv[], wchar_t* envp[]) {
  char** argv8 = (char**)malloc((argc + 1) * sizeof(char*));
  for (int i = 0; i < argc; i++) {
    argv8[i] = to_utf8(argv[i]);
  }
  argv8[argc] = NULL;
  return swish_run(argc, argv8, 0);
}
#else
int main(int argc, const char *argv[]) {
  return swish_run(argc, argv, 0);
}
#endif
