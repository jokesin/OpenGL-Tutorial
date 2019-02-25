#version 430

in vec4 vs_fs_colour;

layout(location = 0) out vec4 fColour;

void main()
{
    //fColour = vec4(1.0f, 0.0f, 1.0f, 1.0f);
	fColour = vs_fs_colour;
}
