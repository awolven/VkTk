/*
Copyright 2019 Andrew Kenneth Wolven <awolven@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
#version 450

layout (triangles) in;
layout (triangle_strip, max_vertices = 3) out;
layout (location = 0) in vec3 fragColorIn[];
layout (location = 0) out vec3 fragColorOut;

layout(set = 0, binding = 1) uniform UniformBufferObject {
  mat4 proj;
  vec4 rayOrigin;
  vec4 rayDir;
} ubo;

void vertex_out(int i, bool highlight) {
  gl_Position = ubo.proj * gl_in[i].gl_Position;
  if (highlight) {
    fragColorOut = vec3(0.0, 0.0, 1.0);
  } else {
    fragColorOut = fragColorIn[i];
  }
  EmitVertex();
}

void vertices_out(bool highlight) {
  for (int i = 0; i < 3; i++) {
    vertex_out(i, highlight);
  }
  EndPrimitive();
}

void main()
{

  vec3 edge1, edge2;
  vec3 input0, input1, input2;
  vec3 h, s, q;
  float a, f, u, v;
  const float EPSILON = 0.0000001;

  input0=gl_in[0].gl_Position.xyz;
  input1=gl_in[1].gl_Position.xyz;
  input2=gl_in[2].gl_Position.xyz;

  edge1 = input1 - input0;
  edge2 = input2 - input0;

  h = cross((ubo.rayDir.xyz), edge2);
  a = dot(edge1, h);

  if (abs(a) < EPSILON) {
    vertices_out(false);
    return;
  }

  f = 1.0/a;
  s = ubo.rayOrigin.xyz - input0;
  u = f * dot(s, h);
  if (u < 0.0 || u > 1.0) {
    vertices_out(false);
    return;
  }
  q = cross(s, edge1);
  v = f * dot(ubo.rayDir.xyz, q);
  if (v < 0.0 || u + v > 1.0) {
    vertices_out(false);
    return;
  }
  float t = f * dot(edge2, q);
  if ( t > EPSILON ) { // ray intersection
    vertices_out(true);
    return;
  } else {
    vertices_out(false);
    return;
  }
}


