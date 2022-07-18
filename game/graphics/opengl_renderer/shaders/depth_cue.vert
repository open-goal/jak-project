#version 430 core

layout (location = 0) in vec2 xy;
layout (location = 1) in vec2 st;

uniform vec4 u_color;
uniform vec2 u_vert_offset;
uniform vec2 u_uv_offset;

out flat vec4 fragment_color;
out vec2 tex_coord;

void main() {
    // Calculate color
    vec4 color = u_color;
    color *= 2; // correct
    
    fragment_color = color;

    // Pass on texture coord
    tex_coord = st + u_uv_offset;

    // Calculate vertex position
    vec4 position = vec4(xy.x, xy.y, 1.0, 1.0);
    position.xy += u_vert_offset;
    position.xy = (position.xy * 2) - 1.0; // convert from [0,1] to clip-space

    gl_Position = position;
}
