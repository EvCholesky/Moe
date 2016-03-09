#import "Basic"

/*
AddNums :: (nA : int, nB : int) -> int
{
	return nA + nB;
}

PrintIf :: (n : int)
{
	if (n < 2)	
		PrintInt(1);
	else if (n == 2)
		PrintInt(2);
	else
		PrintInt(3);
}

TestPointers :: ()
{
	n : s64 = 4;
	pN := *n;
	ppN := *pN;
	PrintInt(@pN);

	n = 3;
	PrintInt(@pN);

	@pN = 6;
	PrintInt(n);

	@@ppN = 8;
	PrintInt(n);
}

STest :: struct
{
	m_n : s8;
}
*/


main :: () -> int 
{
	/*
	FormatStr :: "SomeConst = %d, %d";
	SomeConst :: -3*2;

	//TypedConst : s32 : 16;
	printf(FormatStr, SomeConst, -3*2);
	*/

	SFoo :: struct
	{
		NumElements :: 5;
		ConstConst : s8 : 55;

		aN : [NumElements] int;
	}

	foo : SFoo;
	cElements := SFoo.NumElements;
	printf("NumElements = %d, %d\n", cElements, SFoo.ConstConst);

	ConstZero : s8 : 0;
	ConstThree : s8 : 3;
	ArraySize :: ConstThree;
	aN : [ArraySize] int;

	aN[ConstZero] = 2;
	iN : s32;	
	while iN < ArraySize
	{
		printf("aN[%d] = %d\n", iN, aN[iN]);
		++iN;
	}

 // Tess: "what do trees eat for breakfast?"
 // Robyn: "They eat sunshine and rain"
 // Tess: "No, they eat chinese food."

 // Tess: "He has a blue leaf in his car"
 // Robyn: (seeing an air freshener) "Oh a smell tree?"
 // Tess: "Yeah a smell tree"
 // Robyn: "What do you think it smells like?"
 // Tess: "Corndogs."

	// for i : 2..4 {}
	// for 2..4 {}
	// for array {}
	// for * array {}

	// pV : * void; doesn't work

	return 1;
}


