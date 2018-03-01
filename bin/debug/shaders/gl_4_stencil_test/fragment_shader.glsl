#version 430

in VERTEX {
	vec3 normal;
	vec4 color;
}vertex;

out vec4 color;

void main()
{
    color = vertex.color * (0.1 + abs(vertex.normal.z));
}
