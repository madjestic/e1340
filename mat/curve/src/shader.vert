#version 430 core

#define M_PI 3.1415926535897932384626433832795

layout(location = 0) in float alpha;
layout(location = 1) in vec3 color;
layout(location = 2) in vec3 normal;
layout(location = 3) in vec3 uvCoords;
layout(location = 4) in vec3 vPosition;

uniform mat4 camera;
uniform mat4 persp;
uniform mat4 xform;
//uniform vec3 sunP;

// Output data ; will be interpolated for each fragment.
out VS_OUT {
	float A;
	vec3  N;
	vec3  Ng;
	vec3  Cd;
	vec3  uv;
	vec3  P;
    //vec3  sunP;
} vs_out;

float s1  = 1.0f;
float s2  = 10.0f;
float far = 20000.0f;

float f1 (in float x, in float s)
{
	return (-s*log (x));
}

float f2 (in float x, in float s)
{
	return (-pow(s*log (x), 2.0f));
}

float mixF (in float x, in float far)
{
	return (cos (clamp (x/(-far) + M_PI, 0.0f, M_PI)) + 1.0f);
}

float log10(in float x)
{
	float far  = 10.0f;
	float result = (10.0f / log(far)) * log(x);
	return(result);
}

void main()
{
	mat3 viewRot =
		mat3( camera[0].xyz
			, camera[1].xyz
			, camera[2].xyz );
	
	mat4 cameraRot =
		mat4 ( camera[0]
			 , camera[1]
			 , camera[2]
			 , vec4(0,0,0,1));

	mat3 xformRot =
		mat3 ( xform[0].xyz
			 , xform[1].xyz
			 , xform[2].xyz );	
	
	vs_out.A  = alpha;
	//N  = normalize(perspRot * viewRot * xformRot * normal);
	vs_out.N  = normalize(xformRot * normal);
	vs_out.Ng = normalize(normal);
	vs_out.Cd = color;
	vs_out.uv = uvCoords;
	vs_out.P  =  transpose(xform)[3].xyz;
	//sunP = SunP;
	//P  = (transpose(xform)[3].xyz) * viewRot;
	//P  = vec3(.0, .0, -1.0);
	//P  =  (cameraRot * xform)[3].xyz;

	vec4 position = vec4(vPosition,1.0);	

	gl_Position
		= persp
		* cameraRot
		* xform
		* position;

	float x = length(gl_Position.xyz);
	gl_Position.z = mix (f1(x, s1), f2(x, s2), mixF(x, far));
	// gl_Position.z = -log10(length(gl_Position.xyz));
}
