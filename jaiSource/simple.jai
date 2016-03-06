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

	ArraySize :: ConstThree;
	aN : [ArraySize] int;

	iN : s32;	
	while iN < ArraySize
	{
		printf("aN[%d] = %d\n", iN, aN[iN]);
		++iN;
	}

/*
	SFoo :: struct
	{
		m_n : s8 = 7;
		m_g := 2.2;
	    m_aN : [2] STest;
		m_pFoo : * SFoo;
	}

	foo : SFoo;
	foo.m_aN[0].m_n = 3;
	foo.m_aN[1].m_n = 5;
	printf("aN[0] = %d, an[1] = %d\n", foo.m_aN[0].m_n, foo.m_aN[1].m_n);

	foo.m_pFoo = *foo;

	printf("m_n = %d, m_g = %f\n", foo.m_n, foo.m_g);
	foo.m_n = 12;
	foo.m_g = 12.5;

	printf("m_n = %d, m_g = %f\n", foo.m_n, foo.m_g);

	pFoo : * SFoo = *foo;

	pFoo.m_n = 22;
	pFoo.m_g = 123.45;

	printf("m_n = %d, m_g = %f\n", pFoo.m_n, pFoo.m_g);

	printf("m_n = %d, m_g = %f\n", pFoo.m_pFoo.m_pFoo.m_n, pFoo.m_pFoo.m_pFoo.m_g);

	aN : [4] s32;	

	i : s32 = 0;
	// compiler explodes with "s32 i := 0"

	while i < 4
	{
		printf("init[%d] = %d\n", i, aN[i]);
		aN[i] = i * 2;
		++i;
	}

	apN : [4] * s32;
	i = 0;
	while i < 4
	{
		printf("%d\n", aN[i]);
		apN[i] = *aN[i];
		++i;
	}

	i = 0;
	while i < 4
	{
		printf("%d\n", @apN[i]);	
		++i;
	}

	//pN : * s32 = *aN[0];
	pN : * s32 = aN;
	pN2 := pN;
	//pN = aN;
	i = 0;
	while i < 4
	{
		printf("*aN[i] = %p, pN = %p, pN2 = %p\n", *aN[i], pN, pN2);	
		++i;
		++pN;
		pN2 = pN2 + 1;
	}

	pNTest : * s32 = aN + 4;
	printf("test = %p\n", pNTest);

	//pNBegin := aN;  // error - what should initialize to an array do?
	pNBegin : * s32 = aN;
	while pN2 != aN // can't compare pointer to array
	{
		printf("%p, %p\n", pN, pN2);
		--pN;
		pN2 = pN2 - 1;
	}

	paN : * [4] s32 = *aN; 
	(@paN)[1] = 1;
	printf("%d\n", aN[1]);

	i : s32;
	nDec : int;
	g : float;
	gDec : float64;
	while (i < 10)
	{
		printf("i=%d, nDec=%d, g=%f, gDec = %f\n", i, nDec, g, gDec);
		//i = i+1;
		++i;
		--nDec;
		++g;
		--gDec;
	}

	n : int;
	nPreInc := ++n;
	n = 0;
	nPreDec := --n;
	n = 0;
	nPostInc := n++;
	nPostPostInc := n;
	n = 0;
	nPostDec := n--;
	nPostPostDec := n;
	printf("preInc=%d preDec = %d,   postInc=%d->%d, postDec=%d->%d\n", nPreInc, nPreDec, nPostInc, nPostPostInc, nPostDec, nPostPostDec);
	*/

	// for i : 2..4 {}
	// for 2..4 {}
	// for array {}
	// for * array {}

	

	return 1;
}

ConstThree :: 3;

/*
{
	printf("printf %d, %f\n\n", 2, 6.7);

	// pV : * void; doesn't work
	pB : * u8 = null; 
	PrintString(pB);
	if !pB
		pB = "Was Null";

	PrintPointer(pB);
	if pB != null
		{PrintString("notnull");}
	PrintString(pB);

	PrintString("Hello World.");
	pChz := "test string";
	if pChz
		PrintString(pChz);
	else
		printString("pChz == null");

	nByte: s8 = 10 + 12;
	PrintByte(nByte); 


	nFoo : s64 = AddNums(2, 10);
	gTest := 2.2;
	PrintFloat(gTest);
	PrintFloat(-gTest);
	PrintInt(nFoo);
	PrintInt(-nFoo);

	PrintBool(1 < 2);

	PrintInt(1 | 0);
	PrintBool(true & false);
	PrintBool(true | false);

	nSeven := 7;
	PrintInt(7 | 12);
	PrintInt(7 & 9);

	nFoo3 := nFoo & 1;
	n128 := 128;
	nFoo4 := n128 << 1;
	nFoo5 := n128 >> 2;
	PrintInt(nFoo4);
	PrintInt(nFoo5);

	PrintInt(1 << 7);
	PrintInt(256 >> 2);

	PrintInt(-1);
	PrintInt(-1 + 1);
	PrintIf(1);
	PrintIf(2);
	PrintIf(30);

	fTrue := true;
	printBool(!fTrue);
	printBool(!!fTrue);

	TestPointers();
	return 1;
}
*/