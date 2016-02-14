/* Copyright (C) 2015 Evan Christensen
|
| Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated 
| documentation files (the "Software"), to deal in the Software without restriction, including without limitation the 
| rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit 
| persons to whom the Software is furnished to do so, subject to the following conditions:
| 
| The above copyright notice and this permission notice shall be included in all copies or substantial portions of the 
| Software.
| 
| THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE 
| WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
| COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR 
| OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. */

#pragma once

#define SW_HIDE					0

#if 1
#define WIN32_LEAN_AND_MEAN
#define OEMRESOURCE
#define NOATOM
#define NOCLIPBOARD
#define NOCTLMGR
#define NOMSG
#define NOSERVICE
#define NOSOUND
#define NOTEXTMETRIC
#define NOWH
#define NOWINOFFSETS
#define NOCOMM
#define NODRAWTEXT
#define NOGDI
#define NOKERNEL
#define NOUSER
#include "windows.h"

#else // just decls

typedef unsigned long DWORD;
typedef DWORD *LPDWORD;

typedef char * LPSTR;
typedef const char * LPCSTR;

typedef void * HANDLE;

typedef int BOOL;
typedef unsigned char BYTE;
typedef unsigned short WORD;
typedef BYTE * LPBYTE;
typedef void * LPVOID;

struct STARTUPINFOA
{
	DWORD   cb;
    LPSTR   lpReserved;
    LPSTR   lpDesktop;
    LPSTR   lpTitle;
    DWORD   dwX;
    DWORD   dwY;
    DWORD   dwXSize;
    DWORD   dwYSize;
    DWORD   dwXCountChars;
    DWORD   dwYCountChars;
    DWORD   dwFillAttribute;
    DWORD   dwFlags;
    WORD    wShowWindow;
    WORD    cbReserved2;
    LPBYTE  lpReserved2;
    HANDLE  hStdInput;
    HANDLE  hStdOutput;
    HANDLE  hStdError;
};
typedef STARTUPINFOA * LPSTARTUPINFOA;

struct PROCESS_INFORMATION 
{
    HANDLE hProcess;
    HANDLE hThread;
    DWORD dwProcessId;
    DWORD dwThreadId;
};
typedef PROCESS_INFORMATION * LPPROCESS_INFORMATION;


struct SECURITY_ATTRIBUTES 
{
    DWORD nLength;
    void * lpSecurityDescriptor;
    BOOL bInheritHandle;
};
typedef SECURITY_ATTRIBUTES * LPSECURITY_ATTRIBUTES;

#define STARTF_USESHOWWINDOW	0x00000001
#define INFINITE				0xFFFFFFFF  // Infinite timeout

#define EWC_WINBASEAPI
//#define EWC_WINAPI
#define EWC_WINBASEAPI2 __declspec(dllimport)
#define EWC_WINAPI __stdcall

EWC_WINBASEAPI 
BOOL 
EWC_WINAPI
CreateProcessA(
    LPCSTR lpApplicationName,
    LPSTR lpCommandLine,
    LPSECURITY_ATTRIBUTES lpProcessAttributes,
    LPSECURITY_ATTRIBUTES lpThreadAttributes,
    BOOL bInheritHandles,
    DWORD dwCreationFlags,
    LPVOID lpEnvironment,
    LPCSTR lpCurrentDirectory,
    LPSTARTUPINFOA lpStartupInfo,
    LPPROCESS_INFORMATION lpProcessInformation);

EWC_WINBASEAPI2 DWORD EWC_WINAPI WaitForSingleObject(HANDLE hHandle, DWORD dwMilliseconds);
EWC_WINBASEAPI DWORD EWC_WINAPI GetLastError(void);
EWC_WINBASEAPI BOOL EWC_WINAPI GetExitCodeProcess(HANDLE hProcess, LPDWORD lpExitCode);
EWC_WINBASEAPI BOOL EWC_WINAPI CloseHandle(HANDLE hObject);
#endif // just decls