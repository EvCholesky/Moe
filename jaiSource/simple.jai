
#import "Basic"

AddNums :: (nA : int, nB : int) -> int
{
	return nA + nB;
}

main :: ()
{
	nAck := NReturn();
	nFoo := AddNums(2, 5);
}