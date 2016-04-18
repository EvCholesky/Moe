#foreign_library "GL"
#foreign_library "opengl32"
#foreign_library "..\..\external\glfw\lib\glfw3"
#foreign_library "user32"
#foreign_library "gdi32"
#foreign_library "shell32"

SRect :: struct 
{
    m_left : s32;
    m_top : s32;
    m_right : s32;
    m_bottom : s32;
}

CreateWindow :: (dX : s64, dY : s64, pChzName : & u8, ppVHwnd : && void) -> bool #foreign;
ClearWindow :: (uRed : float, uGreen : float, uBlue : float, uAlpha : float) -> void #foreign;
UpdateWindowEvents :: () -> void #foreign;
SwapBuffers_JAI :: (pVHwnd : & void) -> void #foreign;

FGetNextEvent :: (pEvent: & SEvent) -> bool #foreign;

EVENTK :: enum s32 
{  // If we set this to u8, our packing will stop matching C's.
    Keyboard,
    TextInput,
    Window,
    Quit,
}

KEYCODE :: enum u32 
{
    Unknown: 0,
    ArrowLeft : 1,
    ArrowRight : 2,
    ArrowUp : 3,
    ArrowDown : 4,
    Shift : 5,
    Escape : 6,
    MouseButtonLeft : 7,
    MouseButtonRight : 8,

    Enter : 10,

    F1:  11,
    F2:  12,
    F3:  13,
    F4:  14,
    F5:  15,
    F6:  16,
    F7:  17,
    F8:  18,
    F9:  19,
    F10: 20,
    F11: 21,
    F12: 22,
    F13: 23,
    F14: 24,
    F15: 25,
    F16: 26,
}

EDGES :: enum u32
{
	Off,
	Release,
	Hold,
	Press	
}

SEvent :: struct 
{
    m_eventk : EVENTK = EVENTK.nil;

    // If keyboard event:
    m_edges : EDGES;
    m_keycode : KEYCODE = KEYCODE.nil;
    m_nTextInput : u32;
}
