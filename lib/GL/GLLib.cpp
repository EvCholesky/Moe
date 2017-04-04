#define EWC_TYPES_IMPLEMENTATION

#include "EwcTypes.h"

#include <WindowsStub.h>
#include <mmsystem.h>
#include <GLFW/glfw3.h>


enum EVENTK : s32   // JB - If we set this to u8, our packing will stop matching C's.
{
    EVENTK_Keyboard,
	EVENTK_Joystick,
    EVENTK_TextInput,
    EVENTK_Window,
    EVENTK_Quit,

	EWC_MAX_MIN_NIL(EVENTK)
};

enum KEYCODE : s32 
{
    KEYCODE_Unknown = 0,
    KEYCODE_ArrowLeft = 1,
    KEYCODE_ArrowRight = 2,
    KEYCODE_ArrowUp = 3,
    KEYCODE_ArrowDown = 4,
    KEYCODE_Shift = 5,
    KEYCODE_Escape = 6,
    KEYCODE_MouseButtonLeft = 7,
    KEYCODE_MouseButtonRight = 8,

    KEYCODE_Enter = 10,

    KEYCODE_F1 =  11,
    KEYCODE_F2 =  12,
    KEYCODE_F3 =  13,
    KEYCODE_F4 =  14,
    KEYCODE_F5 =  15,
    KEYCODE_F6 =  16,
    KEYCODE_F7 =  17,
    KEYCODE_F8 =  18,
    KEYCODE_F9 =  19,
    KEYCODE_F10 = 20,
    KEYCODE_F11 = 21,
    KEYCODE_F12 = 22,

	KEYCODE_Space = 30,
	KEYCODE_Apostrophe = 31,
	KEYCODE_Comma = 32,
	KEYCODE_Minus = 33,
	KEYCODE_Period = 34,
	KEYCODE_Slash = 35,
	KEYCODE_0 = 36,
	KEYCODE_1 = 37,
	KEYCODE_2 = 38,
	KEYCODE_3 = 39,
	KEYCODE_4 = 40,
	KEYCODE_5 = 41,
	KEYCODE_6 = 42,
	KEYCODE_7 = 43,
	KEYCODE_8 = 44,
	KEYCODE_9 = 45,
	KEYCODE_Semicolon = 46,
	KEYCODE_Equal = 47,
	KEYCODE_A = 48,
	KEYCODE_B = 49,
	KEYCODE_C = 50,
	KEYCODE_D = 51,
	KEYCODE_E = 52,
	KEYCODE_F = 53,
	KEYCODE_G = 54,
	KEYCODE_H = 55,
	KEYCODE_I = 56,
	KEYCODE_J = 57,
	KEYCODE_K = 58,
	KEYCODE_L = 59,
	KEYCODE_M = 60,
	KEYCODE_N = 61,
	KEYCODE_O = 62,
	KEYCODE_P = 63,
	KEYCODE_Q = 64,
	KEYCODE_R = 65,
	KEYCODE_S = 66,
	KEYCODE_T = 67,
	KEYCODE_U = 68,
	KEYCODE_V = 69,
	KEYCODE_W = 70,
	KEYCODE_X = 71,
	KEYCODE_Y = 72,
	KEYCODE_Z = 73,
	KEYCODE_LeftBracket = 74,
	KEYCODE_Backslash = 75,
	KEYCODE_RightBracket = 76,
	KEYCODE_GraveAccent = 77,

	KEYCODE_JoypadButton1 = 80,
	KEYCODE_JoypadButton2 = 81,
	KEYCODE_JoypadButton3 = 82,
	KEYCODE_JoypadButton4 = 83,
	KEYCODE_JoypadButton5 = 84,
	KEYCODE_JoypadButton6 = 85,
	KEYCODE_JoypadButton7 = 86,
	KEYCODE_JoypadButton8 = 87,
	KEYCODE_JoypadButton9 = 88,
	KEYCODE_JoypadButton10 = 89,
	KEYCODE_JoypadButton11 = 90,
	KEYCODE_JoypadButton12 = 91,
	KEYCODE_JoypadButton13 = 92,
	KEYCODE_JoypadButton14 = 93,
	KEYCODE_JoypadButton15 = 94,
	KEYCODE_JoypadButton16 = 95,
	KEYCODE_JoypadButton17 = 96,
	KEYCODE_JoypadButton18 = 97,
	KEYCODE_JoypadButton19 = 98,
	KEYCODE_JoypadButton20 = 99,

	EWC_MAX_MIN_NIL(KEYCODE)
};

enum EDGES : u32
{
	EDGES_Off,
	EDGES_Release,
	EDGES_Hold,
	EDGES_Press	
};

struct SEvent // tag=event
{
					SEvent()
						:m_eventk(EVENTK_Nil)
						,m_edges(EDGES_Off)
						,m_keycode(KEYCODE_Unknown)
						,m_nTextInput(0)
						{ ; }

    EVENTK			m_eventk;
    EDGES			m_edges;
    KEYCODE			m_keycode;
    u32				m_nTextInput;
};

class SEventFifo
{
public:
					SEventFifo()
					:m_iEventFront(0)
					,m_cEvent(0)
						{ ; }

	SEvent *		PEventPushBack();
	SEvent *		PEventFront();
	void			PopFront();
	int				C() const 
						{ return m_cEvent; }

	static const int s_cEventMax = 100;
	SEvent		m_aEvent[s_cEventMax];
	int			m_iEventFront;
	int			m_cEvent;
};

static SEventFifo s_evfifo;

static int s_cJoystickConnected = 0;
static int s_aiJoystick[GLFW_JOYSTICK_LAST];

SEvent * SEventFifo::PEventPushBack()
{
	if (!EWC_FVERIFY(m_cEvent < s_cEventMax, "EventFifo overflow"))
		return PEventFront();

	int iEvent = (m_iEventFront + m_cEvent) % s_cEventMax;
	++m_cEvent;

	auto pEvent = &m_aEvent[iEvent];
	new (pEvent) SEvent;
	return pEvent;
}

SEvent * SEventFifo::PEventFront()
{
	return &m_aEvent[m_iEventFront];
}

void SEventFifo::PopFront()
{
	if (!EWC_FVERIFY(m_cEvent > 0, "SEventFifo underflow"))
		return;

	m_iEventFront = (m_iEventFront + 1) % s_cEventMax;
	--m_cEvent;
}

KEYCODE KeycodeFromGlfwKey(int nKey)
{
	struct SKeyPair // tag = keyp 
	{
		int			m_nKeyGlfw;
		KEYCODE		m_keycode;
	};

	SKeyPair s_aKeyp [] = 
	{
		{ GLFW_KEY_LEFT,	KEYCODE_ArrowLeft},
		{ GLFW_KEY_RIGHT,	KEYCODE_ArrowRight},
		{ GLFW_KEY_UP,		KEYCODE_ArrowUp},
		{ GLFW_KEY_DOWN,	KEYCODE_ArrowDown},
		{ GLFW_KEY_ESCAPE,	KEYCODE_Escape},

		{ GLFW_KEY_ENTER,	KEYCODE_Enter},

		{ GLFW_KEY_F1,		KEYCODE_F1},
		{ GLFW_KEY_F2,		KEYCODE_F2},
		{ GLFW_KEY_F3,		KEYCODE_F3},
		{ GLFW_KEY_F4,		KEYCODE_F4},
		{ GLFW_KEY_F5,		KEYCODE_F5},
		{ GLFW_KEY_F6,		KEYCODE_F6},
		{ GLFW_KEY_F7,		KEYCODE_F7},
		{ GLFW_KEY_F8,		KEYCODE_F8},
		{ GLFW_KEY_F9,		KEYCODE_F9},
		{ GLFW_KEY_F10,		KEYCODE_F10},
		{ GLFW_KEY_F11,		KEYCODE_F11},
		{ GLFW_KEY_F12,		KEYCODE_F12},

		{ GLFW_KEY_SPACE,	KEYCODE_Space },
		{ GLFW_KEY_APOSTROPHE,	KEYCODE_Apostrophe },
		{ GLFW_KEY_COMMA,	KEYCODE_Comma },
		{ GLFW_KEY_MINUS,	KEYCODE_Minus },
		{ GLFW_KEY_PERIOD,	KEYCODE_Period },
		{ GLFW_KEY_SLASH,	KEYCODE_Slash },
		{ GLFW_KEY_0,	KEYCODE_0 },
		{ GLFW_KEY_1,	KEYCODE_1 },
		{ GLFW_KEY_2,	KEYCODE_2 },
		{ GLFW_KEY_3,	KEYCODE_3 },
		{ GLFW_KEY_4,	KEYCODE_4 },
		{ GLFW_KEY_5,	KEYCODE_5 },
		{ GLFW_KEY_6,	KEYCODE_6 },
		{ GLFW_KEY_7,	KEYCODE_7 },
		{ GLFW_KEY_8,	KEYCODE_8 },
		{ GLFW_KEY_9,	KEYCODE_9 },
		{ GLFW_KEY_SEMICOLON,	KEYCODE_Semicolon },
		{ GLFW_KEY_EQUAL,	KEYCODE_Equal },
		{ GLFW_KEY_A,	KEYCODE_A },
		{ GLFW_KEY_B,	KEYCODE_B },
		{ GLFW_KEY_C,	KEYCODE_C },
		{ GLFW_KEY_D,	KEYCODE_D },
		{ GLFW_KEY_E,	KEYCODE_E },
		{ GLFW_KEY_F,	KEYCODE_F },
		{ GLFW_KEY_G,	KEYCODE_G },
		{ GLFW_KEY_H,	KEYCODE_H },
		{ GLFW_KEY_I,	KEYCODE_I },
		{ GLFW_KEY_J,	KEYCODE_J },
		{ GLFW_KEY_K,	KEYCODE_K },
		{ GLFW_KEY_L,	KEYCODE_L },
		{ GLFW_KEY_M,	KEYCODE_M },
		{ GLFW_KEY_N,	KEYCODE_N },
		{ GLFW_KEY_O,	KEYCODE_O },
		{ GLFW_KEY_P,	KEYCODE_P },
		{ GLFW_KEY_Q,	KEYCODE_Q },
		{ GLFW_KEY_R,	KEYCODE_R },
		{ GLFW_KEY_S,	KEYCODE_S },
		{ GLFW_KEY_T,	KEYCODE_T },
		{ GLFW_KEY_U,	KEYCODE_U },
		{ GLFW_KEY_V,	KEYCODE_V },
		{ GLFW_KEY_W,	KEYCODE_W },
		{ GLFW_KEY_X,	KEYCODE_X },
		{ GLFW_KEY_Y,	KEYCODE_Y },
		{ GLFW_KEY_Z,	KEYCODE_Z },
		{ GLFW_KEY_LEFT_BRACKET,	KEYCODE_LeftBracket },
		{ GLFW_KEY_BACKSLASH,	KEYCODE_Backslash },			// '\'
		{ GLFW_KEY_RIGHT_BRACKET,	KEYCODE_RightBracket },
		{ GLFW_KEY_GRAVE_ACCENT,	KEYCODE_GraveAccent },		// `
	};

	SKeyPair * pKeypMax = EWC_PMAC(s_aKeyp);
	for (SKeyPair * pKeyp = s_aKeyp; pKeyp != pKeypMax; ++pKeyp)
	{
		if (pKeyp->m_nKeyGlfw == nKey)
			return pKeyp->m_keycode;
	}

	return KEYCODE_Unknown;
}

static void GlfwKeyboardCallback(GLFWwindow * pWindow, int nKey, int nScancode, int nAction, int nMods)
{
	KEYCODE keycode = KeycodeFromGlfwKey(nKey);

	EDGES edges;
	switch (nAction)
	{
		case GLFW_PRESS:	edges = EDGES_Press;	break;
		case GLFW_RELEASE:	edges = EDGES_Release;	break;
		default: return; // GLFW_REPEAT
	}

	auto pEvent = s_evfifo.PEventPushBack();
	pEvent->m_eventk = EVENTK_Keyboard;
	pEvent->m_edges = edges;
	pEvent->m_keycode = KeycodeFromGlfwKey(nKey);
}

static void GlfwMouseButtonCallback(GLFWwindow * pWindow, int nButton, int nAction, int mods)
{
	/*
	if ((nAction == GLFW_PRESS) | (nAction == GLFW_RELEASE))
	{
		EDGES edges = nAction == GLFW_PRESS ? EDGES_Press : EDGES_Release;
		s_pInman->HandleKeyCallback(
					ButkFromGlfwMouseButton(nButton), 
					edges, 
					GrfkeymodFromGlfwMod(mods));

		if (nButton == GLFW_MOUSE_BUTTON_LEFT)
		{
			if (nAction == GLFW_PRESS)
			{
				s_pInman->m_posDragStart = s_pInman->m_posMouseCursor;
			}
			s_pInman->m_posDragEnd = s_pInman->m_posMouseCursor;
		}
	}
	*/
}

static void GlfwMouseCursorCallback(GLFWwindow * pWindow, double xCursor, double yCursor)
{
	/*
	CViewport * pViewp = CViewport::PViewp(0);

	CVec2 posCursor((F32)xCursor, pViewp->m_rectBounds.DY() - (F32)yCursor);
	static F32 s_sMovedEpsilonSq = 0.001f;
	s_pInman->m_fMouseHasMoved |= GLengthSq(posCursor - s_pInman->m_posMouseCursor).F32() > s_sMovedEpsilonSq;
	s_pInman->m_posMouseCursor = posCursor;

	CInputBinding *	pInbindUi = s_pInman->Pinbind(INBINDK_PlayerOne);
	if (pInbindUi->Edges(s_pInman, BIPUT_DragButton, INREAD_Peek) != EDGES_Off)
	{
		s_pInman->m_posDragEnd = s_pInman->m_posMouseCursor;
	}*/
}

static void GlfwCharCallback(GLFWwindow * pWindow, unsigned int c)
{
//	printf("char %c\n",c);
    //if ((c > 0) & (c <= 255))
     //   ImGui::GetIO().AddInputCharacter((char)c);
}

static void GlfwScrollCallback(GLFWwindow * pWindow, double dX, double dY)
{
    //ImGuiIO& io = ImGui::GetIO();
    //io.MouseWheel = (dY != 0.0f) ? dY > 0.0f ? 1 : - 1 : 0;           // Mouse wheel: -1,0,+1
}

static void GlfwErrorCallback(int nError, const char* pChz)
{
	printf("Glfw Error: (%d) %s\n", nError, pChz);
}

typedef void (*pfnGlProc)(void);
extern "C" pfnGlProc PFnGlProcLookup(const char * pChzProcname)
{
	auto pFnGlProc = glfwGetProcAddress(pChzProcname);
	auto nGlError = glGetError();
	if (nGlError != GL_NO_ERROR)
	{
		printf("GL Error: %d\n", nGlError);
	}
	return pFnGlProc;
}

enum JOYCONS // JOYstick CONnection State
{
	JOYCONS_Disconnected,
	JOYCONS_Connected,

	JOYCONS_Nil = -1
};

struct SJoystick // tag = Joy
{

	JOYCONS			m_joycons;

	int				m_iJoy;			// GLFW joystick token GLFW_JOYSTICK_1 .. GLFW_JOYSTICK_LAST
	int				m_cGAxis;
	int				m_cBButton;
	const float *	m_aGAxis;		// allocated by GLFL, scope valid until controller disconnect
	const u8 *		m_aBButton;		// allocated by GLFL, scope valid until controller disconnect

	float *			m_aGAxisPrev;	// allocted locally, delete upon controller disconnect
	u8 *			m_aBButtonPrev;	// allocted locally, delete upon controller disconnect
};

struct SJoystickManager
{
					SJoystickManager();

	SJoystick		 m_aJoy[GLFW_JOYSTICK_LAST];

	SJoystickManager *	m_pJoymanNext;
};

SJoystickManager::SJoystickManager()
:m_pJoymanNext(nullptr)
{ 
	SJoystick * pJoyMax = EWC_PMAC(m_aJoy);
	for (SJoystick * pJoy = m_aJoy; pJoy != pJoyMax; ++pJoy)
	{
		pJoy->m_iJoy = int(pJoy - m_aJoy);
		pJoy->m_joycons = JOYCONS_Disconnected;

		pJoy->m_cGAxis = 0;
		pJoy->m_cBButton = 0;
		pJoy->m_aGAxis = nullptr;
		pJoy->m_aBButton = nullptr;

		pJoy->m_aGAxisPrev = nullptr;
		pJoy->m_aBButtonPrev = nullptr;
	}
}

static SJoystickManager * s_pJoymanRoot = nullptr;



static void SetJoycons(SJoystick * pJoy, JOYCONS joycons)
{
	if (pJoy->m_joycons == joycons)
		return;

	pJoy->m_joycons = joycons;
	if (joycons == JOYCONS_Connected)
	{
		const char * pChzName = glfwGetJoystickName(pJoy->m_iJoy);
		printf("joystick \"%s\" connected\n", pChzName);

		pJoy->m_aGAxis = glfwGetJoystickAxes(pJoy->m_iJoy, &pJoy->m_cGAxis);
		pJoy->m_aBButton = glfwGetJoystickButtons(pJoy->m_iJoy, &pJoy->m_cBButton);

		pJoy->m_aGAxisPrev = new float[pJoy->m_cGAxis];
		pJoy->m_aBButtonPrev = new u8[pJoy->m_cBButton];

		auto pGAxisPrevMax = &pJoy->m_aGAxisPrev[pJoy->m_cGAxis];
		for (auto pGAxisPrev = pJoy->m_aGAxisPrev; pGAxisPrev != pGAxisPrevMax; ++pGAxisPrev)
		{
			*pGAxisPrev = 0.0f;
		}

		auto pBButtonPrevMax = &pJoy->m_aBButtonPrev[pJoy->m_cBButton];
		for (auto pBButtonPrev = pJoy->m_aBButtonPrev; pBButtonPrev != pBButtonPrevMax; ++pBButtonPrev)
		{
			*pBButtonPrev = GLFW_RELEASE;
		}
	}
	else
	{
		pJoy->m_aGAxis = nullptr;
		pJoy->m_aBButton = nullptr;
		pJoy->m_cGAxis = 0;
		pJoy->m_cBButton = 0;
		delete[] pJoy->m_aGAxisPrev;
		delete[] pJoy->m_aBButtonPrev;
	}
}

static void GlfwJoystickCallback(int iJoy, int event)
{
	JOYCONS joycons = JOYCONS_Nil;
    if (event == GLFW_CONNECTED)
    {
		joycons = JOYCONS_Connected;
    }
    else if (event == GLFW_DISCONNECTED)
    {
		joycons = JOYCONS_Disconnected;
    }

	if (joycons != JOYCONS_Nil)
	{
		SJoystickManager * pJoyman = s_pJoymanRoot;
		while (pJoyman)
		{
			SetJoycons(&pJoyman->m_aJoy[iJoy], joycons);
			pJoyman = pJoyman->m_pJoymanNext;
		}
	}
}

extern "C" void * CreateJoystickManager()
{
	SJoystickManager * pJoyman = new SJoystickManager;
	if (!s_pJoymanRoot)
	{
		glfwSetJoystickCallback(GlfwJoystickCallback);
	}

	for (int iJoy = 0; iJoy < EWC_DIM(pJoyman->m_aJoy); ++iJoy)
	{
		JOYCONS joycons = (glfwJoystickPresent(iJoy) != 0) ? JOYCONS_Connected : JOYCONS_Disconnected;
		SetJoycons(&pJoyman->m_aJoy[iJoy], joycons);
	}

	pJoyman->m_pJoymanNext = s_pJoymanRoot;
	s_pJoymanRoot = pJoyman;
	return pJoyman;
}

extern "C" void UpdateJoystickManager(void * pVJoyman)
{
	auto pJoyman = (SJoystickManager *)pVJoyman;
	for (int iJoy = 0; iJoy < EWC_DIM(pJoyman->m_aJoy); ++iJoy)
	{
		auto pJoy = &pJoyman->m_aJoy[iJoy];

		for (int iBButton = 0; iBButton < pJoy->m_cBButton; ++iBButton)
		{
			pJoy->m_aBButton = glfwGetJoystickButtons(pJoy->m_iJoy, &pJoy->m_cBButton);

			if (pJoy->m_aBButton[iBButton] != pJoy->m_aBButtonPrev[iBButton])
			{
				printf("joy(%d) button %d: %d -> %d\n", iJoy, iBButton, pJoy->m_aBButtonPrev[iBButton], pJoy->m_aBButton[iBButton]);
				pJoy->m_aBButtonPrev[iBButton] = pJoy->m_aBButton[iBButton];
			}
		}
	}
	pJoyman = pJoyman->m_pJoymanNext;
}

extern "C" void CreateWindow_MOE(s64 dX, s64 dY, const char * pChzName, void ** ppVHwnd)
{
	glfwSetErrorCallback(GlfwErrorCallback);

	if (!glfwInit())
		exit(0);

	GLFWwindow * pWindow = glfwCreateWindow((int)dX, (int)dY, pChzName, nullptr, nullptr);
	*ppVHwnd = pWindow;

	glfwSetKeyCallback(pWindow, GlfwKeyboardCallback);
	glfwSetMouseButtonCallback(pWindow, GlfwMouseButtonCallback);
	glfwSetCursorPosCallback(pWindow, GlfwMouseCursorCallback);
    glfwSetScrollCallback(pWindow, GlfwScrollCallback);
    glfwSetCharCallback(pWindow, GlfwCharCallback);

	glfwMakeContextCurrent(pWindow);
	glfwSwapInterval(0);

	printf("CreateWindow %s (%lldx%lld)\n", pChzName, dX, dY);
}

extern "C" void ClearWindow(float uRed, float uGreen, float uBlue, float uAlpha)
{
//	int dX, dY;
//	glfwGetFramebufferSize(pWindow, &dX, &dY);

	//glViewport(0, 0, dX, dY);
	glClearColor (uRed, uGreen, uBlue, uAlpha);
	glClear(GL_COLOR_BUFFER_BIT);
}

extern "C" void SwapBuffers_MOE(void * pVHwnd)
{
	auto pWindow = (GLFWwindow *)pVHwnd;
	glfwSwapBuffers(pWindow);
}

extern "C" s32 GetMonitorRefresh(void * pVHwnd)
{
	GLFWmonitor * pMonitor = glfwGetPrimaryMonitor();

	if (pMonitor)
	{
		const GLFWvidmode * pVidmode = glfwGetVideoMode(pMonitor);
		if (pVidmode)
			return pVidmode->refreshRate;
	}
	return 0;
}

extern "C" s64 CTickPerSecond()
{
    LARGE_INTEGER lrgintFrequency;
    QueryPerformanceFrequency(&lrgintFrequency);
    return lrgintFrequency.QuadPart;
}

extern "C" s64 CTickWallClock()
{
    LARGE_INTEGER lrgint;
    QueryPerformanceCounter(&lrgint);
    return lrgint.QuadPart;
}

extern "C" bool FTrySetTimerResolution(u32 msResolution)
{
    return timeBeginPeriod(msResolution) == TIMERR_NOERROR;
}

extern "C" void UpdateWindowEvents()
{
	glfwPollEvents();
}

extern "C" bool FGetNextEvent(SEvent * pEvent)
{
	if (s_evfifo.C() == 0)
		return false;

	auto pEventFront = s_evfifo.PEventFront();
	*pEvent = *pEventFront;
	s_evfifo.PopFront();

	return true;
}