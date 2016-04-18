#foreign_library "Basic"

PrintFloat :: (g : float) #foreign;
PrintInt :: (n : int) #foreign;
PrintByte :: (n : s8) #foreign;
PrintBool :: (f : bool) #foreign;
PrintPointer :: (pB : & u8) #foreign;
PrintString :: (pCh : & u8) #foreign;
printf :: (pCh : &u8, ..) -> s32 #foreign;

HANDLE :: typedef & void; 	// This actually needs to be pointer-sized, so s32 in 32-bit, 
						//  but lots of people use integers too, arrrrgh!!

HWND :: typedef HANDLE;
HDC  :: typedef HANDLE;

SStartupInfo :: struct 
{
    m_cb : s32;
    m_lpReserved : & u8;
    m_lpDesktop : & u8;
    m_lpTitle : & u8;
    m_dwX : s32;
    m_dwY : s32;
    m_dwXSize : s32;
    m_dwYSize : s32;
    m_dwXCountChars : s32;
    m_dwYCountChars : s32;
    m_dwFillAttribute: s32;
    m_dwFlags : s32;
    m_wShowWindow : s16;
    m_cbReserved2 : s16;
    m_lpReserved2 : & u8;
    m_hStdInput : HANDLE;
    m_hStdOutput : HANDLE;
    m_hStdError : HANDLE;
}
