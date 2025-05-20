// This is a sample Metal file for testing purposes.

#include <metal_stdlib>
using namespace metal;

struct MyData {
    float value;
    int id;
};

kernel void myKernel(device MyData* data [[buffer(0)]],
                     uint id [[thread_position_in_grid]]) {
    data[id].value *= 2.0;
}

float myHelperFunction(float x, float y) {
    return x + y;
}

vertex float4 vertexShader(const device packed_float3* vertex_array [[buffer(0)]],
                           unsigned int vid [[vertex_id]]) {
    return float4(vertex_array[vid], 1.0);
}

fragment half4 fragmentShader(float4 P [[position]]) {
    return half4(P.x, P.y, P.z, 1.0);
}
