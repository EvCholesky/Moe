#foreign_library "Basic"

PrintFloat :: (g : float) #foreign;
PrintInt :: (n : int) #foreign;
PrintByte :: (n : s8) #foreign;
PrintBool :: (n : bool) #foreign;

NReturn :: () -> int
{
	return 34;
}

