// Sample WGSL file for testing

alias MyVec = vec4<f32>;
alias AnotherVec = vec2<f32>;

struct VertexInput {
    @location(0) position: MyVec,
    @location(1) uv: vec2<f32>,
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
};

struct MyUniforms {
    mvp: mat4x4<f32>,
    color: MyVec,
};

@group(0) @binding(0) var<uniform> u_mvp: mat4x4<f32>;
@group(0) @binding(1) var<uniform> u_color: MyVec;
@group(1) @binding(0) var my_texture: texture_2d<f32>;
@group(1) @binding(1) var my_sampler: sampler;

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    out.position = u_mvp * in.position;
    out.uv = in.uv;
    return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    return u_color * textureSample(my_texture, my_sampler, in.uv);
}

@compute @workgroup_size(8, 8, 1)
fn cs_main(@builtin(global_invocation_id) global_id: vec3<u32>) {
    // A simple compute shader example
    let x: u32 = global_id.x;
    // Do compute work...
}

fn helper_function(val: f32) -> f32 {
    return val * 2.0;
}

// Another struct for good measure
struct AnotherStruct {
    data: array<f32, 4>,
}
