
#import "Basic"

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

main :: () -> int 
{
//	nAck := NReturn();

	nByte: s8 = 10 + 12;
	PrintByte(nByte); 

	nFoo : s64 = AddNums(2, 10);
	gTest := 2.2;
	PrintFloat(gTest);
	PrintInt(nFoo);

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

	return 1;
	//return 100 - nAck + nFoo;
}
