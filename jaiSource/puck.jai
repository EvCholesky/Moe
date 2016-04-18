#import "GL"
#import "Basic"

s_dXWindow :: 1280;
s_dYWindow :: 720;

FHandleEvents :: () -> bool
{
	a : int = 1;	
	b : int = 1;	

	event : SEvent;
	while FGetNextEvent(&event)
	{
		printf("Event: keycode=%d, edges = %d\n", event.m_keycode, event.m_edges);
		if event.m_keycode == KEYCODE.Escape && event.m_edges == EDGES.Press
		{
			return false;
		}
	}

	return true;
}

main :: () -> int
{
	pWindow : HWND;
	CreateWindow(s_dXWindow, s_dYWindow, "Puck", &pWindow);

	fShouldClose := false;
	while fShouldClose == false
	{
		fShouldClose |= !FHandleEvents();

		ClearWindow(0, 0.5, 1, 1);

        SwapBuffers(pWindow);
        // sleep_milliseconds(5);
        UpdateWindowEvents();
	}
	return 1;
}