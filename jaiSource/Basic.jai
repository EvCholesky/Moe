#foreign_library "Basic"

PrintFloat :: (g : float) #foreign;
PrintInt :: (n : int) #foreign;
PrintByte :: (n : s8) #foreign;
PrintBool :: (f : bool) #foreign;
PrintPointer :: (pB : * u8) #foreign;
PrintString :: (pCh : * u8) #foreign;
printf :: (pCh : *u8, ..) -> s32 #foreign;

NReturn :: () -> int
{
	return 34;
}

