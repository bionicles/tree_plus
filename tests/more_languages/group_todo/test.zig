// test.zig
const std = @import("std");

pub fn add(a: i32, b: i32) i32 {
    return a + b;
}

test "add function" {
    std.testing.expect(add(2, 3) == 5);
}
