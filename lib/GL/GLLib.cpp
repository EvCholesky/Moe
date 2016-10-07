#define EWC_TYPES_IMPLEMENTATION

#include "EwcTypes.h"
#include <GLFW/glfw3.h>

enum EVENTK : s32   // JB - If we set this to u8, our packing will stop matching C's.
{
    EVENTK_Keyboard,
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

SEvent * SEventFifo::PEventPushBack()
{
	if (!EWC_FVERIFY(m_cEvent < s_cEventMax, "EventFifo overflow"))
		return PEventFront();
	++m_cEvent;

	int iEvent = (m_iEventFront + m_cEvent) % s_cEventMax;

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

extern "C" void CreateWindow(s64 dX, s64 dY, const char * pChzName, void ** ppVHwnd)
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