#version 430

uniform mat4 view_matrix;
uniform mat4 projection_matrix;

layout(location = 0) in vec4 position;
layout(location = 1) in vec4 color;
layout(location = 2) in vec3 normal;

// model_matrix будет использована для трансформации каждой модели
// mat4 занимает последовательно 4 места, => она будет в 3,4,5,и 6 прозициях
layout(location = 3) in mat4 model_matrix;

out VERTEX {
	vec3 normal;
	vec4 color;
}vertex;

void main()
{	
	mat4 model_view_matrix = view_matrix * model_matrix;
    gl_Position = projection_matrix * (model_view_matrix * position);
	
	//преобразуем нормали с помощью подматрицы 3х3 из model_view_matrix
	vertex.normal = mat3(model_view_matrix) * normal;
	
	vertex.color = color;
}
