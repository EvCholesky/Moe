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

#include "ByteCode.h"
#include "CodeGen.h"
#include "Lexer.h"
#include "Parser.h"
#include "UnitTest.h"
#include "Util.h"
#include "Workspace.h"

#ifdef _WINDOWS
#include "WindowsStub.h"
#endif

using namespace EWC;

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
	printf("    -bytecode : Run bytecode tests\n");
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

			arypChzOptions.Append("link"); // first option (name of exe) is ignored

			if (work.m_optlevel == OPTLEVEL_Release)
			{
				static const char * s_aChzOptimizedDebug[] =
				{
					#if _WINDOWS
					"/debug",
					"/incremental:no",
					#else // POSIX
					#endif
				};

				arypChzOptions.Append(s_aChzOptimizedDebug, EWC_DIM(s_aChzOptimizedDebug));
			}
			else
			{
				#if _WINDOWS
				static const char * s_aChzOptimizedRelease[] =
				{
					"/debug",
				};

				arypChzOptions.Append(s_aChzOptimizedRelease, EWC_DIM(s_aChzOptimizedRelease));
				#endif
			}

			const char * pChzLinkerFull = nullptr;

			#if EWC_X64
				#if _WINDOWS
				#if _MSC_VER == 1900
					static const char * s_pChzCommand = "C:/Program Files (x86)/Microsoft Visual Studio 14.0/VC/bin/amd64/link.exe";

					static const char * s_apChzDefaultPaths[] =
					{
						"c:/Program Files (x86)/Windows Kits/10/lib/10.0.10150.0/ucrt/x64",
						"c:/Program Files (x86)/Microsoft Visual Studio 14.0/VC/lib/amd64",
						"c:/Program Files (x86)/Windows Kits/8.1/lib/winv6.3/um/x64",
						"c:/code/moe/external/glfw/lib/x64"
					};
				#else
					static const char * s_pChzCommand = "C:/Program Files (x86)/Microsoft Visual Studio/2017/Professional/VC/Tools/MSVC/14.11.25503/bin/Hostx64/x64/link.exe";

					static const char * s_apChzDefaultPaths[] =
					{
						"c:/PROGRA~2/WI3CF2~1/10/Lib/100162~1.0/ucrt/x64",
						"C:/PROGRA~2/MIB055~1/2017/PROFES~1/VC/Tools/MSVC/1411~1.255/lib/x64",
						"c:/PROGRA~2/WI3CF2~1/8.1/Lib/winv6.3/um/x64",
						"c:/code/moe/external/glfw/lib/x64"
					};
				#endif
					static const char * s_pChzCommandLld = "C:/Code/llvm38/cmade64/bin/lld-link.exe";



					// Note: Don't string wrap paths with spaces - llvm will do that for us (and double wrap paths)

					// use the following to generate the 8.3 dos path
					//		cmd /c for %A in ("C:\somepathhere") do @echo %~sA
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
					#if _WINDOWS
					"/subsystem:console",
					"/machine:x64",
					"/nologo",
					"/NODEFAULTLIB:MSVCRT.lib",
					"/NODEFAULTLIB:MSVCRTD.lib",
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
				#if _WINDOWS
				#if _MSC_VER == 1900
					static const char * s_pChzCommand = "C:/Program Files (x86)/Microsoft Visual Studio 14.0/VC/bin/link.exe";
				#else
					static const char * s_pChzCommand = "C:/Program Files (x86)/Microsoft Visual Studio/2017/Professional/VC/Tools/MSVC/14.11.25503/bin/Hostx64/x64/link.exe";
				#endif

					static const char * s_apChzDefaultPaths[] =
					{
						"\"c:/Program Files (x86)/Windows Kits/10/lib/10.0.10150.0/ucrt/x86\"",
						"\"c:/Program Files (x86)/Microsoft Visual Studio 14.0/VC/lib\"",
						"\"c:/Program Files (x86)/Windows Kits/8.1/lib/winv6.3/um/x86\"",
						"\"c:/code/moe/external/glfw/lib/win32\""
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
			#ifdef _WINDOWS
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

			// current moe object file
			arypChzOptions.Append(work.m_pChzObjectFilename);

			// output filename
			char aCozOutput[1024];
			const char * pChzOutputPath;	
			const char * pChzOutputFile;	
			const char * pChzOutputExt;	
			(void) EWC::CBCopyCoz(work.m_pChzObjectFilename, aCozOutput, EWC_DIM(aCozOutput));
			PathSplitDestructive(aCozOutput, EWC_DIM(aCozOutput), &pChzOutputPath, &pChzOutputFile, &pChzOutputExt);

			#if _WINDOWS
				static const char * s_pChzMoeLibPath = "c:/Code/moe";
				static const char * s_pChzCRTLibraryDebug = "msvcrtd.lib";
				static const char * s_pChzCRTLibraryRelease = "msvcrt.lib";
				static const char * s_pChzLibOption = "";
				static const char * s_pChzLibPathOption = "/LIBPATH:";
				static const char * s_pChzLibExtension = ".lib";
				static const char * s_pChzOutputFileOption = "/OUT:\"testall\"";
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

				arypChzOptions.Append(s_pChzOutputFileOption);	
				arypChzOptions.Append(pChzOutputFile);
			#endif

			//arypChzOptions.Append(s_pChzOutputFileOption);	
			//arypChzOptions.Append("\"testall\"");

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
			#if _WINDOWS
			FormatCoz(&strbufMoeLib, "%s%s%s%s", s_pChzLibPathOption, s_pChzMoeLibPath, s_pChzMoeLibBit, pChzMoeLibOpt);
			#else
			FormatCoz(&strbufMoeLib, "%s%s%s%s", s_pChzLibPathOption, s_pChzMoeLibPath, pChzMoeLibOpt, s_pChzMoeLibBit);
			#endif

			//arypChzOptions.Append(s_pChzLibPathOption);
			arypChzOptions.Append(strbufMoeLib.m_pCozBegin);

			pCozWorking = strbufMoeLib.m_pCozAppend;
			if (pCozWorking != pCozWorkingMac)
				++pCozWorking;

			// NOTE: This is a bit of a mess. We're not really handling library ordering properly (we need to track
			//  library->library dependencies, but this has us limping along for now - I believe the problem was
			//  that we depend on libraries that depend on msvcrtd, but it was being pulled in first. (and the objects 
			//  were not needed at the time and discarded) 
			//  see: http://eli.thegreenplace.net/2013/07/09/library-order-in-static-linking


			for (int ipChz = 0; ipChz < EWC_DIM(s_apChzDefaultPaths); ++ipChz)
			{
				EWC::SStringBuffer strbufLib(pCozWorking, pCozWorkingMac - pCozWorking);
				FormatCoz(&strbufLib, "%s%s", s_pChzLibPathOption, s_apChzDefaultPaths[ipChz]);

				arypChzOptions.Append(pCozWorking);
				pCozWorking = strbufLib.m_pCozAppend;
				if (pCozWorking != pCozWorkingMac)
					++pCozWorking;
				//arypChzOptions.Append(s_pChzLibPathOption);
				//arypChzOptions.Append(s_apChzDefaultPaths[ipChz]);
			}

			arypChzOptions.Append(nullptr);
			printf("link: \n\"%s\" ",pChzLinkerFull);
			//for (const char ** ppChzIt = &arypChzOptions[1]; ppChzIt != arypChzOptions.PMac(); ++ppChzIt)
			for (const char ** ppChzIt = &arypChzOptions[1]; *ppChzIt; ++ppChzIt)
			{
				printf("%s ", *ppChzIt);
			}
			printf("\n");

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

	if (comline.FHasCommand("-bytecode"))
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

		BCode::BuildTestByteCode(&work, &alloc);
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
	}
	return 0;
}