#import "Basic"

TestOperators :: ()
{
	printf("\nOperators:\n");

	nPre : int;
	nPost : int;

	while nPre < 5
	{
		printf("pre = %d, post = %d\n", ++nPre, nPost++);	
	}

	fTrue := true;
	printf("true = %d, !true = %d, !!true = %d\n", fTrue, !fTrue, !!fTrue);

	printf("6 | 9 = %d, 6 & 2 = %d\n", 6 | 9, 6 & 2);

	n64 := 64;
	printf("64>>2=%d, 64<<2=%d\n", n64>>2, n64<<2);
}

TestPointers :: ()
{
	printf("\nPointers:\n");

	n : s64 = 456;
	pN := *n;
	ppN := *pN;
	printf("@pN = %d, n = %d\n",@pN, n);

	n = 123;
	printf("@pN = %d, n = %d\n",@pN, n);

	@pN = 600;
	printf("@pN = %d, n = %d\n",@pN, n);

	@@ppN = 890;
	printf("@pN = %d, n = %d\n",@pN, n);

	pB : * u8 = null; 
	fIsNull := (pB == null);
	fNotPtr := !pB;
	printf("%p == null, fIsNull=%d, fNotPtr=%d\n", pB, fIsNull, fNotPtr);

	pChz := "test string";
	printf("test string = %s\n", pChz);

	PointStruct :: struct
	{
		m_n : s32;
	}

	pointstruct : PointStruct;
	pPointstruct : * PointStruct = *pointstruct;

	pointstruct.m_n = 1234;
	printf("pointstruct.m_n = %d, pPointstruct.m_n = %d\n", pointstruct.m_n, pPointstruct.m_n);
}

TestNestedMethod :: (n : s32)
{
	NestedMethod :: (n : s32) -> f32
	{
		printf("nested method = %d\n", n);	
		return 2.2;
	}

	printf("\nNested Method: (should print = %d)\n", n);
	i := 0;
	g : f32;
	while i < 3
	{
		g = g + NestedMethod(n);
		++i;
	}

	printf("return = %f\n", g);
}

TestIfElse :: ()
{
	printf("\nIf/Else:\n");

	CLoop :: 4;
	n := 0;
	while n < CLoop
	{
		if n == 0
			printf("(if) %d == 0\n",n);
		else if n < 2
			printf("(else if) %d < 2\n",n);
		else
			printf("(else) %d\n",n);
		++n;
	}

}

TestInitializers :: ()
{
	printf("\nInitializers:\n");	

	cNest :: 3;
	SNestedConst :: struct // tag=nest
	{
		m_n : s8 = 123;
	}

	SInitConst :: struct // tag=init
	{
		m_n : s8 = 7;
		m_g := 2.2;
	    m_aNest : [cNest] SNestedConst;
		m_pInit : * SInitConst;
	}

	SNested :: struct // tag=nest
	{
		m_n : s8 = 123;
		m_uninit : s8 = ---;
	}

	SInit :: struct // tag=init
	{
		m_n : s8 = 7;
		m_g := 2.2;
	    m_aNest : [cNest] SNested;
		m_pInit : * SInit;
	}

	initc : SInitConst;
	init : SInit;

	// perfect test case for nested function!
	iInit := 0;
	printf("initc.m_n = %d, initc.m_g = %f, initc.m_pInit=%p\n", initc.m_n, initc.m_g, initc.m_pInit);
	while iInit < cNest
	{
		printf("initc.m_aNest[%d].m_n = %d (123)\n", iInit, initc.m_aNest[iInit].m_n);
		++iInit;
	}

	iInit = 0;
	printf("init.m_n = %d, init.m_g = %f, init.m_pInit=%p\n", init.m_n, init.m_g, init.m_pInit);
	while iInit < cNest
	{
		printf("init.m_aNest[%d].m_n = %d (123)\n", iInit, init.m_aNest[iInit].m_n);
		++iInit;
	}
}

TestStructures :: ()
{
	printf("\nStructures:\n");

	SChild :: struct
	{
		m_n : s32;
		m_childSelf : * SChild; 	
	}

	SRoot :: struct
	{
		m_child : SChild;
		m_rootSelf : * SRoot;
	}

	root : SRoot;
	root.m_rootSelf = *root;
	root.m_child.m_childSelf = *root.m_child;

	root.m_child.m_n = 1234;

	printf("%d = %d", root.m_child.m_n, root.m_rootSelf.m_rootSelf.m_child.m_childSelf.m_childSelf.m_n);


	SOuter :: struct
	{
	 	m_n : s8;	

	 	SNested :: struct
	 	{
	 		NestedCount :: 3;
	 		m_nNested := 17;
	 	}
	}

	printf("aNest count = %d\n", SOuter.SNested.NestedCount);
	aNest : [SOuter.SNested.NestedCount] SOuter.SNested;

	iNest := 0;
	while iNest < SOuter.SNested.NestedCount
	{
		printf("aNest[%d] = %d\n", iNest, aNest[iNest].m_nNested);
		++iNest;
	}
}

TestRecursion :: ()
{
	printf("\nRecursion:\n");

	NFactorial :: (n : int) -> int
	{
		if n > 1
		{
			return n * NFactorial(n-1);
		}
		return 1;
	}

	printf("4! = %d (24)\n", NFactorial(4));

	MethodA :: (n : int)
	{
		if (n > 0)	
			MethodB(n-1);

	}

	MethodB :: (n : int)
	{
		printf("MethodB = %d\n", n);
		MethodA(n);
	}

	MethodA(5);
}

TestTypedefs :: ()
{
	printf("\nTypedefs:\n");

	n : SLater.IntAlias = 2;
	aN : SLater.QuadArray;

	n := 0;
	while n < SLater.ConstFour
	{
		printf("aN[%d] = %d\n", aN[n], n);
		++n;
	}
}

TestEnums :: ()
{
	printf("\nEnumerations:\n");

	ENUMK :: enum
	{
		Ack : 0,
		Bah : Ack+1,
		Ugh,	
		Guh : Bah + Ugh,
		Four : SLater.ConstFour,
		Final
	}

	enumkTest : ENUMK;
	//enumkTest = 2; // should not work
	//enumkTest = SOuter.MEMB.Two; // should not work

	printf("SLater.MEMB.One = %d\n", SLater.MEMB.One);
	printf("SLater.MEMB.Two = %d\n", SLater.MEMB.Two);
	printf("SLater.MEMB.Three = %d\n", SLater.MEMB.Three);

	printf("Ack = %d (0)\n", ENUMK.Ack);
	printf("Bah = %d (1)\n", ENUMK.Bah);
	printf("Ugh = %d (2)\n", ENUMK.Ugh);
	printf("Guh = %d (3)\n", ENUMK.Guh);
	printf("Four = %d (4)\n", ENUMK.Four);
	printf("Final = %d (5)\n", ENUMK.Final);
	printf("(nil) = %d\n", ENUMK.nil);
	printf("(min) = %d\n", ENUMK.min);
	printf("(last) = %d\n", ENUMK.last);
	printf("(max) = %d\n", ENUMK.max);

	nBah : s32 = -ENUMK.Bah; 
	printf("nBah = %d\n", nBah);

	enumkAck := ENUMK.Ack;
	printf("enumkAck = %d\n", enumkAck);
}

main :: () -> int 
{
	TestOperators();
	TestPointers();
	TestIfElse();
	TestNestedMethod(20);
	TestInitializers();
	TestStructures();
	TestRecursion();
	TestTypedefs();
	TestEnums();

	return 1;
}

SLater :: struct
{
	ConstFour :: 4;

	IntAlias :: typedef s32;
	QuadArray :: typedef [ConstFour] IntAlias;

	// ArrayFail :: typedef [Four] IntAlias; // test this!

	MEMB :: enum
	{
		One : 1,
		Two,
		Three
	}
}

