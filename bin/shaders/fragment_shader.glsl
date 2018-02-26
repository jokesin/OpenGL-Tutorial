#version 430

in vec4 vs_fs_colour;

layout(location = 0) out vec4 fColour;

void main()
{
    fColour = vs_fs_colour;
}
