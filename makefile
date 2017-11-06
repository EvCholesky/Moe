

# makefile for building moe compiler on OSX, use visual studio for Windows builds

#BB - generate two sets of object files? ala vc?
BUILD_DIR= ./build


LLVM_MODULES = core jit native
LLVM_CFLAGS = `llvm-config --cppflags $(LLVM_MODULES)`
LLVM_LDFLAGS = `llvm-config --ldflags $(LLVM_MODULES)`
LLVM_LIBS = `llvm-config --libs $(LLVM_MODULES)`

CC=clang++
CFLAGS= -MMD
LDFLAGS=
SOURCES=Codegen.cpp EwcString.cpp Lexer.cpp Main.cpp Parser.cpp UnitTest.cpp Util.cpp Workspace.cpp
OBJECTS=$(SOURCES:%.cpp=$(BUILD_DIR)/%.o)

# *.d files generated by clang (from option -MMD) so that header file edits rebuild properly
DEPENDS=$(SOURCES:%.cpp=$(BUILD_DIR)/%.d)

EXECUTABLE=moe

#targets 

release: CFLAGS += -02
release: $(SOURCES) $(EXECUTABLE)

debug: CFLAGS += -DDEBUG -g
debug: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

%.o : %.cpp
	$(CC) $(CFLAGS) $< -o $@	

clean:
	rm -f $(BUILD_DIR)/$(EXECUTABLE) $(OBJECTS) $(DEPENDS)

-include $(DEPENDS)