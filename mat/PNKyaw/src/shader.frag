#version 430

uniform float     u_time;
uniform vec2      u_resolution;
uniform sampler2D tex_00;

in vec4 gl_FragCoord;
in float A;
in vec3  N;
in vec3  Ng;
in vec3  Cd;
in vec3  uv;

out vec4 fragColor;

void main()
{
	float x		  = gl_FragCoord.x/u_resolution.x;		 
	float blendF = sin(x*M_PI);
	fragColor     = vec4( Cd.x, Cd.y, Cd.z, A*blendF );
	//fragColor     = vec4( 1,0,0,1 );
}
