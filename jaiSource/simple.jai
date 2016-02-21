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
*/

main :: () -> int 
{
	/*
	n := 56;
	pN := *n;
	printf("%d\n", pN[0]);

	pN[0] = 67;
	printf("%d\n", n);
	*/

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

	paN : * [4] s32 = *aN; 
	(@paN)[1] = 1;
	printf("%d\n", aN[1]);

/*
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