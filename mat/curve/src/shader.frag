#version 430

uniform float     u_time;
uniform vec2      u_resolution;
uniform sampler2D tex_00;

/* in VS_OUT { */
/* 	float A; */
/* 	vec3  N; */
/* 	vec3  Ng; */
/* 	vec3  Cd; */
/* 	vec3  uv; */
/* 	vec3  P; */
/* } vs_in; */

/* vec3 Cd = vs_in.Cd; */

/* in vec4 gl_FragCoord; */
/* in float A; */
/* in vec3  N; */
/* in vec3  Ng; */
in vec3  Cd;
/* in vec3  uv; */

out vec4 fragColor;

void main()
{
	//fragColor = vec4( Cd.x, Cd.y, Cd.z, A );
	fragColor = vec4( Cd.x, Cd.y, Cd.z, 1.0f );
	//fragColor = vec4( 1.0f, 0.0f, 0.0f, 1.0f );
}
