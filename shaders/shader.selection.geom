#version 450

layout (triangles) in;
layout (triangle_strip, max_vertices = 6) out;
layout (location = 0) in float fragColor[3];
layout (location = 0) out vec3 fragColorOut;


layout(set = 0, binding = 1) uniform UniformBufferObject {
  mat4 mxProj;
  vec4 RayOrigin;
  vec4 RayDir;
} ubo;

void vertex_out(int i, bool highlight) {
  gl_Position = ubo.mxProj * gl_in[i].gl_Position;
  if (highlight) {
    fragColorOut = vec3(0.0, 0.0, 1.0);
  } else {
    fragColorOut = vec3(fragColor[0], fragColor[1], fragColor[2]);
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
  //vec4 Picker;
  vec3 h, s, q;
  float a, f, u, v;
  const float EPSILON = 0.0000001;

  //input0=vec3(gl_in[0].gl_Position.x/gl_in[0].gl_Position.w, gl_in[0].gl_Position.y/gl_in[0].gl_Position.w, gl_in[0].gl_Position.z/gl_in[0].gl_Position.w);
  //input1=vec3(gl_in[1].gl_Position.x/gl_in[1].gl_Position.w, gl_in[1].gl_Position.y/gl_in[1].gl_Position.w, gl_in[1].gl_Position.z/gl_in[1].gl_Position.w);
  //input2=vec3(gl_in[2].gl_Position.x/gl_in[2].gl_Position.w, gl_in[2].gl_Position.y/gl_in[2].gl_Position.w, gl_in[2].gl_Position.z/gl_in[2].gl_Position.w);
  input0=gl_in[0].gl_Position.xyz;
  input1=gl_in[1].gl_Position.xyz;
  input2=gl_in[2].gl_Position.xyz;

  edge1 = input1 - input0;
  edge2 = input2 - input0;

  h = cross((ubo.RayDir.xyz), edge2);
  a = dot(edge1, h);
  //Picker.w = determinant(mat3(-(ubo.RayDir.xyz),edge[0],edge[1]));

  if (abs(a) < EPSILON) {
    // if ((abs(Picker.w) < 1.4012985e-45)) {
    //if ((abs(Picker.w) <= 1.0e-2)) {
    // ray is parallel to triangle
    vertices_out(false);
    return;
  }

  f = 1.0/a;
  s = ubo.RayOrigin.xyz - input0;
  u = f * dot(s, h);
  if (u < 0.0 || u > 1.0) {
    vertices_out(false);
    return;
  }
  q = cross(s, edge1);
  v = f * dot(ubo.RayDir.xyz, q);
  if (v < 0.0 || u + v > 1.0) {
    vertices_out(false);
    return;
  }
  float t = f * dot(edge2, q);
  if ( t > EPSILON ) { // ray intersection
    vertices_out(true);
    gl_Position = ubo.mxProj*ubo.RayOrigin;
    fragColorOut = vec3(0.0, 1.0, 0.0);
    EmitVertex();
    gl_Position = ubo.mxProj*ubo.RayOrigin;
    fragColorOut = vec3(0.0, 1.0, 0.0);
    EmitVertex();    
    gl_Position = vec4((ubo.mxProj*(ubo.RayOrigin+ubo.RayDir*t)).xyz, 1.0);
    fragColorOut = vec3(0.0, 1.0, 0.0);
    EmitVertex();
    EndPrimitive();
    return;
  } else {
    vertices_out(false);
    return;
  }
    //intersectionPoint = ubo.RayOrigin.xyz + ubo.RayDir.xyz * t;
    /*if (Picker.w < 0.0) {
      
    Picker.w = -Picker.w;
    edge[2] = input0;
    
  } else {
    
    edge[2] = -input0;
    Picker.x = determinant(mat3(-(ubo.RayDir.xyz), edge[2], edge[1]));
  }

  if (Picker.x < 0.0 || Picker.x > Picker.w) {
    
    vertices_out(false);
    return;

  }

  Picker.y = determinant(mat3(-(ubo.RayDir.xyz), edge[0], edge[2]));
  
  if (Picker.y < 0.0 || Picker.x + Picker.y > Picker.w) {
    
    vertices_out(false);
    return;
  }

  
  Picker.z = determinant(mat3(edge[2], edge[0], edge[1]));
  Picker.z = Picker.z / Picker.w * (ubo.RayOrigin.z);
    */
  //  gl_Position = vec4(0, 0, Picker.z, 1);
  //  gl_Position.zw = gl_Position.zw * mat2(ubo.mxProj[2].yz, ubo.mxProj[3].yz);

  //  gl_PrimitiveID = gl_PrimitiveIDIn;


}


