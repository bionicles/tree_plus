// test.zig
const std = @import("std");

pub fn add(a: i32, b: i32) i32 {
    return a + b;
}

test "add function" {
    std.testing.expect(add(2, 3) == 5);
}

// borrowed this realistic struct example from https://github.com/oven-sh/bun/blob/main/build.zig (MIT License)
const BunBuildOptions = struct {
    is_canary: bool = false,
    base_path: [:0]const u8 = "",

    runtime_js_version: u64 = 0,
    fallback_html_version: u64 = 0,

    tinycc: bool = true,

    pub fn updateRuntime(this: *BunBuildOptions) anyerror!void {
        if (std.fs.cwd().openFile("src/runtime.out.js", .{ .mode = .read_only })) |file| {
            defer file.close();
            const runtime_hash = Wyhash.hash(
                0,
                try file.readToEndAlloc(std.heap.page_allocator, try file.getEndPos()),
            );
            this.runtime_js_version = runtime_hash;
        } else |_| {
            if (!is_debug_build) {
                @panic("Runtime file was not read successfully. Please run `make setup`");
            }
        }
        // omitting more for brevity
    }

    pub fn step(this: BunBuildOptions, b: anytype) *std.build.OptionsStep {
        var opts = b.addOptions();
        opts.addOption(@TypeOf(this.is_canary), "is_canary", this.is_canary);
        // omitting more for brevity
        return opts;
    }
};

pub fn sgemv(
    order: Order,
    trans: Trans,
    m: usize,
    n: usize,
    alpha: f32,
    a: []const f32,
    lda: usize,
    x: []const f32,
    x_add: usize,
    beta: f32,
    y: []f32,
    y_add: usize,
) void {
    //
}