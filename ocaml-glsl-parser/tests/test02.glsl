//#version 450 core

out vec3 position;

uniform mat4 viewMatrix;

void main() {
	mat4 invViewMatrix = inverse(viewMatrix);
	position = invViewMatrix[3].xyz;
}

