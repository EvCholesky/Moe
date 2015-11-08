

#define EWC_TYPES_IMPLEMENTATION
#include "JaiLex.h"
#include "JaiParse.h"
#include "TypeCheck.h"

using namespace EWC;

int main()
{
	TestLexing();
	TestParse();
	TestTypeCheck();

	return 0;
}