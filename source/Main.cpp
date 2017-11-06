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

#define EWC_TYPES_IMPLEMENTATION
#include "EwcTypes.h"

#include "CodeGen.h"
#include "Lexer.h"
#include "Parser.h"
#include "UnitTest.h"
#include "Util.h"
#include "Workspace.h"

#ifdef WIN32
#include "WindowsStub.h"
#endif

using namespace EWC;

extern void TestParse();
extern void TestTypeCheck();
extern void TestCodeGen();

extern void PathSplitDestructive(char * pCozFull, size_t cBMax, const char ** ppCozPath, const char ** ppCozFile, const char ** ppCozExt);

class CCommandLine // tag = comline
{
public:
	struct SCommand // tag = com
	{
		HV				m_hvName;
		const char *	m_pCozValue;
	};

	CCommandLine()
	:m_aryCom()
	,m_pChzFilename(nullptr)
		{ ; }

	void Parse(int cpChzArg, const char * apChzArg[])
	{
		HV hvLlvm = HvFromPCoz("-llvm");

		const char * pChzFilename = nullptr;
		for (int ipChz = 1; ipChz < cpChzArg; ++ipChz)
		{
			const char * pChzArg = apChzArg[ipChz];
			if (pChzArg[0] == '-')
			{
				if (m_aryCom.C() == m_aryCom.CMax())
				{
					printf("Too many arguments. ignoring %s\n", pChzArg);
				}
				else
				{
					SCommand * pCom = m_aryCom.AppendNew();
					pCom->m_hvName = HvFromPCoz(pChzArg);

					if (pCom->m_hvName == hvLlvm)
					{
						if (ipChz + 1 >= cpChzArg)
						{
							printf("expected argument after -llvm\n");
						}
						else
						{

							++ipChz;
							pCom->m_pCozValue = apChzArg[ipChz];
						}
					}
				}
			}
			else
			{
				if (pChzFilename)
					printf("Expected one filename argument.");

				pChzFilename = pChzArg;
			}
		}
		m_pChzFilename = pChzFilename;
	}

	void AppendCommandValues(const char * pChzCommand, CAry<const char *> * paryPCozValues)
	{
		HV hvCommand = HvFromPCoz(pChzCommand);

		for (size_t iCom = 0; iCom < m_aryCom.C(); ++iCom)
		{
			if (m_aryCom[iCom].m_hvName == hvCommand)
			{
				paryPCozValues->Append(m_aryCom[iCom].m_pCozValue);
			}
		}
	}

	bool FHasCommand(const char * pChzCommand)
	{
		HV hvCommand = HvFromPCoz(pChzCommand);

		for (size_t iCom = 0; iCom < m_aryCom.C(); ++iCom)
		{
			if (m_aryCom[iCom].m_hvName == hvCommand)
				return true;
		}
		return false;
	}

	static const int s_cComMax = 50;
	CFixAry<SCommand, s_cComMax>	m_aryCom;
	const char *					m_pChzFilename;
};

void PrintCommandLineOptions()
{
	printf("moe [options] [filename]\n");
	printf("  options:\n");
	printf("	-fast     : faster compilation, worse codegen\n");
	printf("	-help     : Print this message\n");
	printf("	-nolink   : skip the linker step\n");
	printf("	-printIR  : Print llvm's intermediate representation\n");
	printf("    -release  : Generate optimized code and link against optimized local libraries\n");
	printf("    -test     : Run compiler unit tests\n");
	printf("    -useLLD   : Use llvm linker (rather than linke.exe) use this to emit DWARF debug data.\n");
	printf("    -llvm cmd : run an llvm command line\n");
}

int main(int cpChzArg, const char * apChzArg[])
{
#ifdef MOE_DEBUG
	printf("Warning: Compiler was built in debug mode and may be very slow.\n"); 
#endif

	if (cpChzArg < 2)
	{
		PrintCommandLineOptions();
		return 0;
	}

	CCommandLine comline;
	comline.Parse(cpChzArg, apChzArg);

	CFixAry<const char *, CCommandLine::s_cComMax+1> aryPCozLlvm;
	aryPCozLlvm.Append("moe");
	comline.AppendCommandValues("-llvm", &aryPCozLlvm);

	if (comline.FHasCommand("-help"))
	{
		PrintCommandLineOptions();
	}

	GRFCOMPILE grfcompile;
	if (comline.FHasCommand("-printIR"))
	{
		grfcompile.AddFlags(FCOMPILE_PrintIR);
	}

	if (comline.FHasCommand("-fast"))
	{
		grfcompile.AddFlags(FCOMPILE_FastIsel);
	}

	static const int s_cBHeap = 1000 * 1024;
	static const int s_cBError = 100 * 1024;
	u8 * aB = nullptr;

	InitLLVM(&aryPCozLlvm);

	if (comline.m_pChzFilename && !comline.FHasCommand("-test"))
	{
		u8 aBString[1024 * 100];
		CAlloc allocString(aBString, sizeof(aBString));
		StaticInitStrings(&allocString);

		aB = new u8[s_cBHeap];
		CAlloc alloc(aB, s_cBHeap);

		u8 * aBError = new u8[s_cBError];
		CAlloc allocError(aBError, s_cBError); 

		SErrorManager errman(&allocError);
		CWorkspace work(&alloc, &errman);

		if (comline.FHasCommand("-release"))
		{
			work.m_optlevel = OPTLEVEL_Release;
		}

		BeginWorkspace(&work);

#ifdef EWC_TRACK_ALLOCATION
		u8 aBAltrac[1024 * 100];
		CAlloc allocAltrac(aBAltrac, sizeof(aBAltrac));

		CAllocTracker * pAltrac = PAltracCreate(&allocAltrac);
		work.m_pAlloc->SetAltrac(pAltrac);
#endif
		bool fSuccess = FCompileModule(&work, grfcompile, comline.m_pChzFilename);
		ShutdownLLVM();

		// current linker command line:
		// link simple.obj ..\x64\debug\basic.lib libcmt.lib libucrt.lib /libpath:"c:\Program Files (x86)\Windows Kits\10\lib\10.0.10150.0\ucrt\x64" /libpath:"c:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\lib\amd64" /libpath:"c:\Program Files (x86)\Windows Kits\8.1\lib\winv6.3\um\x64" /subsystem:console /machine:x64

		if (fSuccess && !comline.FHasCommand("-nolink"))
		{
			CDynAry<const char *> arypChzOptions(&alloc, BK_Linker, 32);

			static const char * s_pChzPathDebug = "/Debug";
			static const char * s_pChzPathRelease = "/Release";
			const char * pChzMoeLibOpt = (work.m_optlevel == OPTLEVEL_Release) ? s_pChzPathRelease : s_pChzPathDebug;

			static const char * s_aChzOptimizedDebug[] =
			{
				#if WIN32
				"/debug",
				"/incremental:no",
				#else // POSIX
				#endif
			};
			static const char * s_aChzOptimizedRelease[] =
			{
			};
			if (work.m_optlevel == OPTLEVEL_Release)
			{
				arypChzOptions.Append(s_aChzOptimizedDebug, EWC_DIM(s_aChzOptimizedDebug));
			}
			else
			{
				arypChzOptions.Append(s_aChzOptimizedRelease, EWC_DIM(s_aChzOptimizedRelease));
			}

			const char * pChzLinkerFull = nullptr;

			#if EWC_X64
				#if WIN32
					static const char * s_pChzCommand = "C:/Program Files (x86)/Microsoft Visual Studio 14.0/VC/bin/amd64/link.exe";
					static const char * s_pChzCommandLld = "C:/Code/llvm38/cmade64/bin/lld-link.exe";

					static const char * s_apChzDefaultPaths[] =
					{
						"\"c:/Program Files (x86)/Windows Kits/10/lib/10.0.10150.0/ucrt/x64\"",
						"\"c:/Program Files (x86)/Microsoft Visual Studio 14.0/VC/lib/amd64\"",
						"\"c:/Program Files (x86)/Windows Kits/8.1/lib/winv6.3/um/x64\"",
						"c:/code/moe/external/glfw/lib/x64"
					};
				#else
					static const char * s_pChzCommand = "/usr/bin/ld";
					static const char * s_pChzCommandLld = "/code/llvm50/cmade64/bin/lld";

					static const char * s_apChzDefaultPaths[] =
					{
					};
				#endif
				pChzLinkerFull = (comline.FHasCommand("-useLLD")) ? s_pChzCommandLld : s_pChzCommand; 

				static const char * s_pChzMoeLibBit = "/x64";
				static const char * s_apChzCommand[] = 
				{
					"ld",	// first argument is ignored, name of self

					#if WIN32
					"/subsystem:console",
					"/machine:x64",
					"/nologo",
					"/NODEFAULTLIB:MSVCRT.lib",
					"/NODEFAULTLIB:LIBCMTD.lib",
					#else // POSIX
					"-arch",
					"x86_64",
					"-macosx_version_min",
					"10.11", 
					"-lm",
					#endif
				};

				arypChzOptions.Append(s_apChzCommand, EWC_DIM(s_apChzCommand));
			#else
				#if WIN32
					static const char * s_pChzCommand = "C:/Program Files (x86)/Microsoft Visual Studio 14.0/VC/bin/link.exe";

					static const char * s_apChzDefaultPaths[] =
					{
						"\"c:/Program Files (x86)/Windows Kits/10/lib/10.0.10150.0/ucrt/x86\"",
						"\"c:/Program Files (x86)/Microsoft Visual Studio 14.0/VC/lib\"",
						"\"c:/Program Files (x86)/Windows Kits/8.1/lib/winv6.3/um/x86\"",
						"c:/code/moe/external/glfw/lib/win32"
					};
				#else
					static const char * s_pChzCommand = "/usr/bin/ld";

					static const char * s_apChzDefaultPaths[] =
					{
					};
				#endif

				static const char * s_pChzMoeLibBit = "";
				static const char * s_pChzCommandLld = "C:/Code/llvm38/cmade/bin/lld-link.exe";
				pChzLinkerFull = (comline.FHasCommand("-useLLD")) ? s_pChzCommandLld : s_pChzCommand; 
			#endif

			char aChzCwd[1024];
			#ifdef WIN32
				GetCurrentDirectoryA(EWC_DIM(aChzCwd), (char*)aChzCwd);
			#else
				if (!getcwd(aChzCwd, EWC_DIM(aChzCwd)))
				{
					aChzCwd[0] = 0;
					printf("Failed to find the current working directory\n");
				}
			#endif

			char aCozCopy[2048];
			EWC::SStringBuffer strbufCopy(aCozCopy, EWC_DIM(aCozCopy));
			AppendCoz(&strbufCopy, pChzLinkerFull);

			const char * pChzLinkerPath;	
			const char * pChzLinkerFile;	
			PathSplitDestructive(aCozCopy, EWC_DIM(aCozCopy), &pChzLinkerPath, &pChzLinkerFile, nullptr);

			#if WIN32
				static const char * s_pChzMoeLibPath = "c:/Code/moe";
				static const char * s_pChzCRTLibraryDebug = "msvcrtd.lib";
				static const char * s_pChzCRTLibraryRelease = "msvcrt.lib";
				static const char * s_pChzLibOption = "";
				static const char * s_pChzLibPathOption = "/libpath:";
				static const char * s_pChzLibExtension = ".lib";
				static const char * s_pChzOutputFileOption = "/OUT";
				//static const char * s_pChzLibPathFormat = "/libpath:\"%s\" "

				//const char * pChzCRTLibrary = (work.m_optlevel == OPTLEVEL_Release) ? s_pChzCRTLibraryRelease  : s_pChzCRTLibraryDebug;
				arypChzOptions.Append((work.m_optlevel == OPTLEVEL_Release) ? s_pChzCRTLibraryRelease  : s_pChzCRTLibraryDebug);
			#else // POSIX
				static const char * s_pChzMoeLibPath = "/Code/moe";
				static const char * s_pChzCRTLibraryDebug = "-lcrt1.o";
				static const char * s_pChzCRTLibraryRelease = "-lcrt1.o";
				static const char * s_pChzLibOption = "-l";
				static const char * s_pChzLibPathOption = "-L";
				static const char * s_pChzLibExtension = "";
				static const char * s_pChzOutputFileOption = "-o";

				arypChzOptions.Append((work.m_optlevel == OPTLEVEL_Release) ? s_pChzCRTLibraryRelease  : s_pChzCRTLibraryDebug);
			#endif

			// current moe object file
			arypChzOptions.Append(work.m_pChzObjectFilename);

			// output filename
			char aCozOutput[1024];
			const char * pChzOutputPath;	
			const char * pChzOutputFile;	
			const char * pChzOutputExt;	
			(void) EWC::CBCopyCoz(work.m_pChzObjectFilename, aCozOutput, EWC_DIM(aCozOutput));
			PathSplitDestructive(aCozOutput, EWC_DIM(aCozOutput), &pChzOutputPath, &pChzOutputFile, &pChzOutputExt);

			arypChzOptions.Append(s_pChzOutputFileOption);	
			arypChzOptions.Append(pChzOutputFile);

			char aCozWorking[2048];
			char * pCozWorking = aCozWorking;
			char * pCozWorkingMac = EWC_PMAC(aCozWorking);

			// libraries requested by moe
			CWorkspace::SFile ** ppFileMac = work.m_arypFile.PMac();
			for (CWorkspace::SFile ** ppFile = work.m_arypFile.A(); ppFile != ppFileMac; ++ppFile)
			{
				const CWorkspace::SFile & file = **ppFile;
				if (file.m_filek != CWorkspace::FILEK_Library)
					continue;

				EWC::SStringBuffer strbufLib(pCozWorking, pCozWorkingMac - pCozWorking);
				FormatCoz(&strbufLib, "%s%s%s", s_pChzLibOption, file.m_strFilename.PCoz(), s_pChzLibExtension);
				EnsureTerminated(&strbufLib, '\0');

				arypChzOptions.Append(pCozWorking);
				pCozWorking = strbufLib.m_pCozAppend;
				if (pCozWorking != pCozWorkingMac)
					++pCozWorking;
			}

			// build the path for moe libraries			

			EWC::SStringBuffer strbufMoeLib(pCozWorking, pCozWorkingMac - pCozWorking);
			FormatCoz(&strbufMoeLib, "%s%s%s", s_pChzMoeLibPath, pChzMoeLibOpt, s_pChzMoeLibBit);

			arypChzOptions.Append(s_pChzLibPathOption);
			arypChzOptions.Append(strbufMoeLib.m_pCozBegin);

			// NOTE: This is a bit of a mess. We're not really handling library ordering properly (we need to track
			//  library->library dependencies, but this has us limping along for now - I believe the problem was
			//  that we depend on libraries that depend on msvcrtd, but it was being pulled in first. (and the objects 
			//  were not needed at the time and discarded) 
			//  see: http://eli.thegreenplace.net/2013/07/09/library-order-in-static-linking


			for (int ipChz = 0; ipChz < EWC_DIM(s_apChzDefaultPaths); ++ipChz)
			{
				arypChzOptions.Append(s_pChzLibPathOption);
				arypChzOptions.Append(s_apChzDefaultPaths[ipChz]);
			}

			printf("link: \n%s ",pChzLinkerFull);
			for (const char ** ppChzIt = arypChzOptions.A(); ppChzIt != arypChzOptions.PMac(); ++ppChzIt)
			{
				printf("%s ", *ppChzIt);
			}
			printf("\n");

			#if 0 //WIN32
				STARTUPINFOA startupinfo = {};
				startupinfo.cb = sizeof(startupinfo);
				startupinfo.dwFlags = STARTF_USESHOWWINDOW;
				startupinfo.wShowWindow = SW_HIDE;

				PROCESS_INFORMATION processinfo = {};

				printf("Linking:\n");

				// BB - Should switch CreateProcess and GetCurrentDirectory to wide char versions.
				if (CreateProcessA(
						pChzLinkerFull,
						aCozCommandLine,
						0,
						0,
						false,
						0,
						0,
						aChzCwd,
						&startupinfo,
						&processinfo))
				{
					WaitForSingleObject(processinfo.hProcess, INFINITE);

					DWORD nExitCode;
					(void) GetExitCodeProcess(processinfo.hProcess, &nExitCode);
					CloseHandle(processinfo.hProcess);
					CloseHandle(processinfo.hThread);
				}
				else
				{
					DWORD nErrorCode = GetLastError();
				}
			#elif 0 // !WIN32 so... POSIX?

				// Could actually work, was reporting invalid permissions, but I think that's just a bad path.
				//  I think having llvm::Command do all the work is better tho, just checking it in once for a backup.
				pid_t nPid = 	0;
			    char * apChzArgv[] = {(char *)pChzLinkerFile, NULL}; // gonna take some tweaking to get the args right for posix

			    posix_spawn_file_actions_t fileact;
			    posix_spawn_file_actions_t * pFileact = &fileact;
		      	posix_spawn_file_actions_init(pFileact);

				int nStatus = posix_spawn(&nPid, (char *)pChzLinkerPath, pFileact, NULL, apChzArgv, environ);
			    if (nStatus == 0) 
			    {
			        printf("Child pid: %i\n", nPid);
			        if (waitpid(nPid, &nStatus, 0) != -1) 
			        {
			            printf("Child exited with status %i\n", nStatus);
        			} 
        			else 
        			{
			            perror("waitpid");
			        }
			    } 
			    else 
			    {
			        printf("posix_spawn moe error: %s\n", strerror(nStatus));
			    }// POSIX
		    #else // let llvm do the non-portable stuff.
				EWC::CString strError;
				bool fFailed;

				arypChzOptions.Append(nullptr);
		    	int nResult = NExecuteAndWait(
					    		pChzLinkerFull,
								arypChzOptions.A(),
								nullptr, // ppChzEnvp,
								0,	// no timeout
								0,	// no memory limit
								&strError,
								&fFailed);
					
				if (nResult < 0 || fFailed)
				{
					printf("Linker process failed, %s\n", strError.PCoz());
				}
			#endif 
		}

		EndWorkspace(&work);

#ifdef EWC_TRACK_ALLOCATION
		DeleteAltrac(&allocAltrac, pAltrac);
		work.m_pAlloc->SetAltrac(nullptr);
#endif
		StaticShutdownStrings(&allocString);
	}
	if (aB)
	{
		delete[] aB;
	}

	if (comline.FHasCommand("-test"))
	{
		u8 aBString[1024 * 100];
		CAlloc allocString(aBString, sizeof(aBString));
		StaticInitStrings(&allocString);

		aB = new u8[s_cBHeap];
		CAlloc alloc(aB, s_cBHeap);

		u8 * aBError = new u8[s_cBError];
		CAlloc allocError(aBError, s_cBError);

		SErrorManager errman(&allocError);
		CWorkspace work(&alloc, &errman);
		FUnitTestFile(&work, comline.m_pChzFilename);

		StaticShutdownStrings(&allocString);
/*
		TestUnicode();
		TestLexing();
		TestParse();
		TestTypeCheck();
		TestCodeGen();
		printf("passed unit tests\n");
		*/
	}
	return 0;
}