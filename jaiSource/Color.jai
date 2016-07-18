/* Copyright (C) 2015 Evan Christensen
|
| Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated 
| documentation files (the "Software"), to deal in the Software without restriction, including without limitation the 
| rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit 
| persons to whom the Software is furnished to do so, subject to the following conditions:
| 
| The above copyright notice and this permission notice shall be included in all copies or substantial portions of the 
| Software.
| 
| THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE 
| WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
| COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR 
| OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. */

#import "Math"

SColorVec :: typedef SVector4;

SColor :: struct
{
	m_r: u8;
	m_g: u8;
	m_b: u8;
	m_a: u8;
}

ColvecFFromCol :: () -> SColorVec
{
	return VecMul(Vec4Create(cast(f32)m_r, cast(f32)m_g, cast(f32)m_b, (f32)m_a), 1.0/255.0);
}


ColLerp :: (colA: SColor, colB: SColor, uLerp: f32) -> SColor
{
	gR := cast(f32)colA.m_r; GClamp(gR + (cast(f32)colB.m_r - gR) * uLerp, 0, 255);
	gG := cast(f32)colA.m_g; GClamp(gG + (cast(f32)colB.m_g - gG) * uLerp, 0, 255);
	gB := cast(f32)colA.m_b; GClamp(gB + (cast(f32)colB.m_b - gB) * uLerp, 0, 255);
	gA := cast(f32)colA.m_a; GClamp(gA + (cast(f32)colB.m_a - gA) * uLerp, 0, 255);
}

inline CColor		ColLerp(CColor colA, CColor colB, F32 gBlend)
						{
F32 gR = (F32)colA.m_r; ewcClamp<F32>(gR + ((F32)colB.m_r - gR) * gBlend, 0.0f, 255.0f);
							F32 gG = (F32)colA.m_g; ewcClamp<F32>(gG + ((F32)colB.m_g - gG) * gBlend, 0.0f, 255.0f);
							F32 gB = (F32)colA.m_b; ewcClamp<F32>(gB + ((F32)colB.m_b - gB) * gBlend, 0.0f, 255.0f);
							F32 gA = (F32)colA.m_a; ewcClamp<F32>(gA + ((F32)colB.m_a - gA) * gBlend, 0.0f, 255.0f);
							return CColor((U8)gR,(U8)gG,(U8)gB,(U8)gA);	
						}

inline CColorVec	ColvecLerp(CColorVecArg colvecA, CColorVecArg colvecB, CScalarArg gBlend)
						{
							return VecClamp( VecLerp(colvecA, colvecB, gBlend), CColorVec(KZERO), CColorVec(KONE)); 
						}

inline CColor		ColScaleAlpha(CColor col, F32 scale)
						{
							F32 gA = (F32)col.m_a * scale;
							return CColor(col.m_r, col.m_g, col.m_b, (U8)gA);
						}
2