#version 430

uniform mat4 view_matrix;
uniform mat4 projection_matrix;

// позиция и номаль - постоянные вершинные атрибуты
layout(location = 0) in vec4 position;
layout(location = 1) in vec3 normal;

// цвет - атрибут в зависимости от копии (instance)
layout(location = 2) in vec4 color;

// model_matrix будет использована для трансформации каждой модели
// mat4 занимает последовательно 4 места, => она будет в 3,4,5,и 6 прозициях
layout(location = 3) in mat4 model_matrix;

out VERTEX {
	vec3 normal;
	vec4 color;
}vertex;

void main()
{
	//построим model_view_matrix из view_matrix и model_matrix
	mat4 model_view_matrix = view_matrix * model_matrix;
	
	//преобразуем позиции в зависимости от model_view_matrix и затем projection_matrix
	gl_Position = projection_matrix * ( model_view_matrix * position);
	
	//преобразуем нормали с помощью подматрицы 3х3 из model_view_matrix
	vertex.normal = mat3(model_view_matrix) * normal;
	
	//просто прокинем цвет дальше
	vertex.color = color;
}
