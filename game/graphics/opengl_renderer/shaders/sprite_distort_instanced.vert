#version 430 core

layout (location = 0) in vec3 xyz;                  // position from sine table
layout (location = 1) in vec2 st;                   // tex coord from sine table
layout (location = 2) in vec4 instance_xyz_s;       // sprite position + texture S-coord
layout (location = 3) in vec4 instance_scale_t;     // sprite scale + texture T-coord

uniform vec4 u_color;

out flat vec4 fragment_color;
out vec2 tex_coord;

void main() {
    // Pass on fragment color
    fragment_color = u_color;

    // Adjust position and texture coordinates the same way as in the VU program
    //
    // The VU program operated on each "slice" of the sprite separately, which in
    // this case is every 5 vertices. To know the exact transformation to apply
    // for each vertex, we need to know which one it is. The order is always 2
    // vertices scaled by sizeX, 2 scaled by sizeY (sizeZ for tex coords), and
    // finally the center vertex which isn't modified.
    float slice_vert_id = mod(gl_VertexID, 5);

    vec2 texture_coord = vec2(instance_xyz_s.w, instance_scale_t.w);
    if (slice_vert_id < 2) {
      texture_coord += st * instance_scale_t.x;
    } else if (slice_vert_id < 4) {
      texture_coord += st * instance_scale_t.z;
    }

    tex_coord = texture_coord;

    vec3 position = instance_xyz_s.xyz;
    if (slice_vert_id < 2) {
      position += xyz * instance_scale_t.x;
    } else if (slice_vert_id < 4) {
      position += xyz * instance_scale_t.y;
    }

    vec4 transformed = vec4(position, 1.0);

    // correct xy offset
    transformed.xy -= (2048.);
    // correct z scale
    transformed.z /= (8388608);
    transformed.z -= 1;
    // correct xy scale
    transformed.x /= (256);
    transformed.y /= -(128);

    gl_Position = transformed;
}
