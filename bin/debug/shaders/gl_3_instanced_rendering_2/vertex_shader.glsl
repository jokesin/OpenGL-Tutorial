#version 430

// матрицы вида и проекции постоянные на протяжении всего процесса отрисовки
uniform mat4 view_matrix;
uniform mat4 projection_matrix;

// позиция и номаль - постоянные вершинные атрибуты
layout(location = 0) in vec4 position;
layout(location = 1) in vec3 normal;

// Ниже следует описание TBO (Texture Buffer Objects), которые содержат
// по-экземплярный цвет и по-экземплярную матрицу модели
uniform samplerBuffer color_tbo;
uniform samplerBuffer model_matrix_tbo;

out VERTEX {
	vec3 normal;
	vec4 color;
}vertex;

void main()
{
	// Используем gl_InstanceID для получения цвета экземпляра из TBO цвета
	vec4 color = texelFetch(color_tbo, gl_InstanceID);
	
	//С матрицей все немного сложнее : мы не можем сохранить
	//mat4 в TBO. Следовательно, нам следует сохранить каждую матрицу
	//как набор четырех vec4, и затем собрать ее в шейдере.
	//Для начала выберем 4 столбца матрицы ( помним, что матрицы сохраняются
	//в памяти по столбцам )
	vec4 col1 = texelFetch(model_matrix_tbo, gl_InstanceID * 4);
	vec4 col2 = texelFetch(model_matrix_tbo, gl_InstanceID * 4 + 1);
	vec4 col3 = texelFetch(model_matrix_tbo, gl_InstanceID * 4 + 2);
	vec4 col4 = texelFetch(model_matrix_tbo, gl_InstanceID * 4 + 3);
	
	//Теперь соберем 4 столбца в матрицу
	mat4 model_matrix = mat4(col1, col2, col3, col4);
	
	//построим model_view_matrix из view_matrix и model_matrix
	mat4 model_view_matrix = view_matrix * model_matrix;
	
	//преобразуем позиции в зависимости от model_view_matrix и затем projection_matrix
	gl_Position = projection_matrix * ( model_view_matrix * position);
	
	//преобразуем нормали с помощью подматрицы 3х3 из model_view_matrix
	vertex.normal = mat3(model_view_matrix) * normal;
	
	//просто прокинем цвет дальше
	vertex.color = color;
}
