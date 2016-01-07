
#import "Basic"

AddNums :: (nA : int, nB : int) -> int
{
	return nA + nB;
}

main :: () -> int 
{
//	nAck := NReturn();
	//nByte: s8 = 10; doesn't work as we don't know the size of constants during codeGen
	nFoo : s64 = AddNums(2, 10);
	gTest := 2.2;
	PrintFloat(gTest);
	PrintInt(nFoo);

	return 1;
	//return 100 - nAck + nFoo;
}