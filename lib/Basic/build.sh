#Basic.a

mkdir -p build
pushd build > /dev/null

CL="/usr/bin/clang++"
AR="/usr/bin/ar"
COMPILE_OPTIONS_LLVM=`/usr/local/opt/llvm/bin/llvm-config --cxxflags --ldflags --system-libs --libs core mcjit native bitwriter`
COMPILE_DISABLE_WARNINGS="-Wno-nested-anon-types -Wno-missing-field-initializers"
COMPILE_OPTIONS="-DPLATFORM_OSX=1 -g -Wall -Werror -c -O0 -std=c++0x -working-directory=../source" #-I/usr/local/opt/libffi/lib/libffi-3.0.13/include

$CL $COMPILE_OPTIONS $COMPILE_DISABLE_WARNINGS basic.cpp
$AR -q libbasic.a basic.o

popd > /dev/null	
cp ./build/libbasic.a ../../Debug/x64
