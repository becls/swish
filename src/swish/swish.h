// Copyright 2019 Beckman Coulter, Inc.
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

#define SWISH_VERSION "0.0.0"

#ifdef _WIN32
#define WINVER 0x0601 // Windows 7
#define _WIN32_WINNT WINVER
#include "uv.h"
#define SCHEME_IMPORT
#include "scheme.h"
#define strdup _strdup
#undef EXPORT

#if __cplusplus
# if defined(SWISH_IMPORT)
#   define EXPORT extern "C" __declspec (dllimport)
# elif defined(SWISH_STATIC)
#   define EXPORT extern "C"
# else
#   define EXPORT extern "C" __declspec (dllexport)
# endif
#else
# if defined(SWISH_IMPORT)
#   define EXPORT extern __declspec (dllimport)
# elif defined(SWISH_STATIC)
#   define EXPORT extern
# else
#   define EXPORT extern __declspec (dllexport)
# endif
#endif

#else

#include "uv.h"
#include "scheme.h"

#  if __cplusplus
#    define EXPORT extern "C"
#  else
#    define EXPORT extern
#  endif

#endif

EXPORT uv_loop_t* osi_loop;

EXPORT void osi_add_callback_list(ptr callback, ptr args);
EXPORT void osi_add_callback1(ptr callback, ptr arg);
EXPORT void osi_add_callback2(ptr callback, ptr arg1, ptr arg2);
EXPORT void osi_add_callback3(ptr callback, ptr arg1, ptr arg2, ptr arg3);
EXPORT ptr osi_make_error_pair(const char* who, int error);
EXPORT char* osi_string_to_utf8(ptr s, size_t* utf8_len);

EXPORT int swish_run(int argc, const char* argv[], void (*custom_init)(void));
