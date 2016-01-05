
#import "Basic"

AddNums :: (nA : int, nB : int) -> int
{
	return nA + nB;
}

main :: () -> int 
{
	nAck := NReturn();
	nFoo := AddNums(2, 5);
	
	return nAck + nFoo;
}