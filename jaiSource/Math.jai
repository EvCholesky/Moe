#import "Basic"

g_Pi :: 3.1415926535897932;

SVector2 :: struct
{
	m_x: float;
	m_y: float;
}

SVector3 :: struct
{
	m_x: float;
	m_y: float;
	m_z: float;
}

SVector4 :: struct
{
	m_x: float;
	m_y: float;
	m_z: float;
	m_w: float;
}

Vec2Create :: (x: float, y: float) -> SVector2
{
	vec : SVector2;
	vec.m_x = x;
	vec.m_y = y;
	return vec;
}

Vec3Create :: (x: float, y: float, z: float) -> SVector3
{
	vec : SVector3;
	vec.m_x = x;
	vec.m_y = y;
	vec.m_z = z;
	return vec;
}

Vec4Create :: (x: float, y: float, z: float, w: float) -> SVector4
{
	vec : SVector4;
	vec.m_x = x;
	vec.m_y = y;
	vec.m_z = z;
	vec.m_w = w;
	return vec;
}

VecRotate :: (gRadians: float, vec: SVector2) -> SVector2
{
	gSin := sin(gRadians);
	gCos := cos(gRadians);

	//[c -s][ x ]   [ cx - sy ]
	//[s  c][ y ] = [ cy + sx ]

	return Vec2Create(gCos * vec.m_x - gSin * vec.m_y, gCos * vec.m_y + gSin * vec.m_x);
}

VecAdd :: inline (vecLhs: SVector2, vecRhs: SVector2) -> SVector2
{
	return Vec2Create(vecLhs.m_x + vecRhs.m_x, vecLhs.m_y + vecRhs.m_y);
}

VecSub :: inline (vecLhs: SVector2, vecRhs: SVector2) -> SVector2
{
	return Vec2Create(vecLhs.m_x - vecRhs.m_x, vecLhs.m_y - vecRhs.m_y);
}

VecMul :: inline (vec: SVector2, r: f32) -> SVector2
{
	return Vec2Create(vec.m_x * r, vec.m_y * r);
}

VecAdd :: inline (vecLhs: SVector3, vecRhs: SVector3) -> SVector3
{
	return Vec3Create(vecLhs.m_x + vecRhs.m_x, vecLhs.m_y + vecRhs.m_y, vecLhs.m_z + vecRhs.m_z);
}

VecSub :: inline (vecLhs: SVector3, vecRhs: SVector3) -> SVector3
{
	return Vec3Create(vecLhs.m_x - vecRhs.m_x, vecLhs.m_y - vecRhs.m_y, vecLhs.m_z - vecRhs.m_z);
}

VecMul :: inline (vec: SVector3, r: f32) -> SVector3
{
	return Vec3Create(vec.m_x * r, vec.m_y * r, vec.m_z * r);
}

VecAdd :: inline (vecLhs: SVector4, vecRhs: SVector4) -> SVector4
{
	return Vec4Create(
			vecLhs.m_x + vecRhs.m_x,
			vecLhs.m_y + vecRhs.m_y,
			vecLhs.m_z + vecRhs.m_z,
			vecLhs.m_w + vecRhs.m_w);
}

VecSub :: inline (vecLhs: SVector4, vecRhs: SVector4) -> SVector4
{
	return Vec4Create(
			vecLhs.m_x - vecRhs.m_x,
			vecLhs.m_y - vecRhs.m_y,
			vecLhs.m_z - vecRhs.m_z,
			vecLhs.m_w - vecRhs.m_w);
}

VecMul :: inline (vec: SVector4, r: f32) -> SVector4
{
	return Vec4Create(vec.m_x * r, vec.m_y * r, vec.m_z * r, vec.m_w * r);
}

GMin :: inline (gA: f32, gB: f32) -> f32					{ if (gA < gB) return gA; return gB; }
GMax :: inline (gA: f32, gB: f32) -> f32					{ if (gA > gB) return gA; return gB; }
GClamp :: inline (gValue: f32, gMin: f32, gMax: f32) -> f32 { return GMin(GMax(gValue, gMin), gMax); }

