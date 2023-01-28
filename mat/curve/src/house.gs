#version 430 core
layout (points) in;
layout (triangle_strip, max_vertices = 5) out;
//layout (line_strip, max_vertices = 5) out;

in VS_OUT {
	float A;
	vec3  N;
	vec3  Ng;
	vec3  Cd;
	vec3  uv;
	vec3  P;
} gs_in[];

out vec3 Cd;

void build_house(vec4 position)
{
  Cd          = gs_in[0].Cd;
  float s     = 2.0f;
  gl_Position = position + s * vec4(-0.2, -0.2, 0.0, 0.0);	  // 1:bottom-left
  EmitVertex();	  
  gl_Position = position + s * vec4( 0.2, -0.2, 0.0, 0.0);	  // 2:bottom-right
  EmitVertex();
  gl_Position = position + s * vec4(-0.2,  0.2, 0.0, 0.0);	  // 3:top-left
  EmitVertex();
  gl_Position = position + s * vec4( 0.2,  0.2, 0.0, 0.0);	  // 4:top-right
  EmitVertex();
  gl_Position = position + s * vec4( 0.0,  0.4, 0.0, 0.0);	  // 5:top
  EmitVertex();
  EndPrimitive();
}

void main() {    
    build_house(gl_in[0].gl_Position);
}  
