Build notes mostly for myself so I don't have to wander back through the cMake field of landmines.

I know the directory structure here is a bit of a mess, and I'll reorganize it later... but for now I just need this written down.

build with releases from [link](http://llvm.org/releases/download.html)

<h1>Building LLVM libraries with visual studio (tested with source from 3.7.1 tarball)</h1>

* make sure mcAfee real-time scanning is turned off because it will make cMake fail. 

`C:\code\llvm\cmade>cmake ..\src\ -G "Visual Studio 14"`
`C:\code\llvm\cmade64>cmake ..\src\ -G "Visual Studio 14 Win64"`



<h1>Getting clang, lld, and lldb to build for debug info.</h1>

* Unzip llvm 3.8 tarball in ./src/ directory and unzip clang, lld and lldb into respective ./src/tool/ folders. 

* Install python, gnuWin32, ninja and swig as described here: [link](http://lldb.llvm.org/build.html)

	- in addition to the python install we need to build python debug libs (due, I believe, to an errant dependency in lldb 3.8) so download the source and build it in visual studio. (several of the libraries will fail due to missing dependencies but we don't care... move along). There must be some kind of install process to move the debug exes/dlls into the install directory structure but I didn't find it... I just manually copied python_d.exe, python35_d.dll and libs\python35_d.lib into the install directory. (yuck!)

* Run vcvarsall to set up for visual studio command line builds (adding x64 to ensure ninja builds for x64)
	"C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" x64

* Make sure the path includes the following:
	C:\gnuwin32\bin
	C:\Code\swig
	C:\Code\ninja

* Create a cMade64 directory and cmake into it.
	`"c:\Program Files (x86)\CMake\bin\Cmake.exe" -DPYTHON_HOME=C:\Users\Evan\AppData\Local\Programs\Python\Python35 -DCMAKE_BUILD_TYPE=release -G Ninja c:\code\llvm38\src`

	(again, make sure macafee is disabled. sigh.)

	- ATTENTION: make sure the output doesn't contain text like this:
		"Python installation is corrupt. Python support will be disabled for this build."

* Use ninja to build the subprojects
	`ninja clang`
	`ninja lld`
	`ninja lldb`

Currenly only supporting win32  with debug info as 64bit ld doesn't work.

* Reminder of all the broken tools wrt. debug info *
- lld doesn't really support x64, dwarf data gets dropped
- neovim can't seem to get it's python installed properly so pyuv is unhappy, :UpdateInstalledPlugins fails and lldb integration is a no-go.
- llvm 3.8's install of lldb has a errant(?) dependency on python_d(and friends) and even after manually creating and copying it's not happy - dies failing to find _ctypes (looks like lldb has a python script to install the debug files, maybe that would help)
- MIEngine can run an instance of lldb (with the console command Debug.MIDebugLaunch), but doesn't seem to find the source and doesn't seem ready for use.