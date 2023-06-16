// Count the number of times any of the characters specified as command
// line options appear on standard input.

const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var hits: i32 = 0;

    const input = try stdin.readUntilDelimiterAlloc(allocator, '\n', 512);
    for (input) |c| {
        for (args) |a| {
            if (c == a[0]) {
                hits += 1;
                break;
            }
        }
    }

    defer allocator.free(input);

    try stdout.print("{d}\n", .{hits});
}
