#include "stb_image.h"
#include "MoeLib.h"

MOE_EXPORT unsigned char * StbiLoad_MOE(char const *filename, int *x, int *y, int *comp, int req_comp)
{
	return stbi_load(filename, x, y, comp, req_comp);
}

