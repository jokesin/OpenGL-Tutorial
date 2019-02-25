#version 430

uniform mat4 model_matrix;
uniform mat4 projection_matrix;

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec4 vColour;

out vec4 vs_fs_colour;

void main()
{
	vs_fs_colour = vColour;	
    gl_Position = projection_matrix * (model_matrix * vPosition);
	//gl_Position = vPosition;
}
