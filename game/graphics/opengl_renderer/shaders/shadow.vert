#version 430 core

layout (location = 0) in vec3 position_in;

void main() {
    // Note: position.y is multiplied by 32 instead of 16 to undo the half-height for interlacing stuff.
    gl_Position = vec4((position_in.x - 0.5) * 16., -(position_in.y - 0.5) * 32, position_in.z * 2 - 1., 1.0);
    // scissoring area adjust
    gl_Position.y *= SCISSOR_ADJUST;
}
