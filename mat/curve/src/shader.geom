//#version 330 core
#version 430
layout (points) in;
layout (line_strip, max_vertices = 2) out;

in VS_OUT {
	float A;
	vec3  N;
	vec3  Ng;
	vec3  Cd;
	vec3  uv;
	vec3  P;
} gs_in[];

void main() {    
    gl_Position = gl_in[0].gl_Position + vec4(-0.1, 0.0, 0.0, 0.0); 
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + vec4( 0.1, 0.0, 0.0, 0.0);
    EmitVertex();
    
    EndPrimitive();
}  
