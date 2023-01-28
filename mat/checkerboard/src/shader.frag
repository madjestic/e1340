#version 430

uniform float     u_time;
uniform vec2      u_resolution;
uniform sampler2D checkerboard;

in vec4 gl_FragCoord;
in float A;
in vec3  N;
in vec3  Ng;
in vec3  Cd;
in vec3  uv;

out vec4 fragColor;

void main()
{
	vec4 font_clr = texture(checkerboard, vec2(uv.x, 1.0-uv.y));
	fragColor     = vec4(vec3(font_clr.r), font_clr.r*2.0);
	//fragColor   = vec4( Cd.x, Cd.y, Cd.z, A );
}