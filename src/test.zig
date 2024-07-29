// The GPLv3 License (GPLv3)

// Copyright © 2024 tusharhero

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

const std = @import("std");
const Lexxer = @import("lexxer.zig").Lexxer;
const Token = @import("tokens.zig").Token;
const Type = @import("tokens.zig").TokenType;
const Parser = @import("parser.zig").Parser;
const Printer = @import("ast.zig").Printer;
const Interpreter = @import("interpreter.zig").Interpreter;

fn test_program(source: []const u8, expected_output: []const u8) !bool {
    const test_allocator = std.testing.allocator;

    var output = std.ArrayList(u8).init(test_allocator);
    defer output.deinit();

    const writer = output.writer();

    var lexxer = try Lexxer.init(test_allocator);
    defer lexxer.deinit();

    var parser = try Parser.init();
    defer parser.deinit();

    var interpreter = try Interpreter.init(
        test_allocator,
        .{ .arraylist = writer },
    );
    defer interpreter.deinit();

    const tokens = try lexxer.scanTokens(source);
    const statements = try parser.parse(tokens);
    try interpreter.interpret(statements);

    return std.mem.eql(u8, output.items, expected_output);
}

test "printing" {
    const code =
        \\print 2;
    ;
    const expected_ouput =
        \\2
        \\
    ;
    try std.testing.expect(try test_program(code, expected_ouput));
}

test "var, printing" {
    const code =
        \\var a = 1+1;
        \\print a;
    ;
    const expected_ouput =
        \\2
        \\
    ;
    try std.testing.expect(try test_program(code, expected_ouput));
}

test "string concatenation" {
    const code =
        \\var zig = "Ziggy";
        \\var lox = "Loxxy";
        \\print zig + lox;
    ;
    const expected_ouput =
        \\ZiggyLoxxy
        \\
    ;
    try std.testing.expect(try test_program(code, expected_ouput));
}

test "scope" {
    const code =
        \\var a = "global a";
        \\var b = "global b";
        \\var c = "global c";
        \\{
        \\  var a = "outer a";
        \\  var b = "outer b";
        \\  {
        \\    var a = "inner a";
        \\    print a;
        \\    print b;
        \\    print c;
        \\  }
        \\  print a;
        \\  print b;
        \\  print c;
        \\} 
        \\print a;
        \\print b;
        \\print c;
    ;
    const expected_ouput =
        \\inner a
        \\outer b
        \\global c
        \\outer a
        \\outer b
        \\global c
        \\global a
        \\global b
        \\global c
        \\
    ;
    try std.testing.expect(try test_program(code, expected_ouput));
}

test "if else" {
    const code =
        \\var a = 1;
        \\var b = 26;
        \\print "Value of a is";
        \\print a;
        \\print "Value of b is";
        \\print b;
        \\if (a > b)
        \\   print "a is larger than b";
        \\else
        \\   print "a is smaller than b";
    ;
    const expected_ouput =
        \\Value of a is
        \\1
        \\Value of b is
        \\26
        \\a is smaller than b
        \\
    ;
    try std.testing.expect(try test_program(code, expected_ouput));
}

test "while loop" {
    const code =
        \\var a = 0;
        \\while (a < 10) {
        \\ print a;
        \\ a = a + 1;
        \\}
    ;
    const expected_ouput =
        \\0
        \\1
        \\2
        \\3
        \\4
        \\5
        \\6
        \\7
        \\8
        \\9
        \\
    ;
    try std.testing.expect(try test_program(code, expected_ouput));
}

test "for loop" {
    const code =
        \\for(var i = 0; i < 10; i = i + 1) {
        \\   print i;
        \\}
    ;
    const expected_ouput =
        \\0
        \\1
        \\2
        \\3
        \\4
        \\5
        \\6
        \\7
        \\8
        \\9
        \\
    ;
    try std.testing.expect(try test_program(code, expected_ouput));
}

test "and, or" {
    const code =
        \\print 1 and 1;
        \\print true and false;
        \\print false or true;
    ;
    const expected_ouput =
        \\1
        \\false
        \\true
        \\
    ;
    try std.testing.expect(try test_program(code, expected_ouput));
}

test "functions no return" {
    const code =
        \\fun hello() {
        \\  print "Hello World!";
        \\}
        \\hello();
        \\
    ;
    const expected_ouput =
        \\Hello World!
        \\
    ;
    try std.testing.expect(try test_program(code, expected_ouput));
}

test "functions return" {
    const code =
        \\fun hello() {
        \\  return "Hello World!";
        \\}
        \\print hello();
        \\
    ;
    const expected_ouput =
        \\Hello World!
        \\
    ;
    try std.testing.expect(try test_program(code, expected_ouput));
}

test "simple recursion" {
    const code =
        \\fun add(a,b,c) {
        \\  if (c == 0) return a + b;
        \\  return add(a,b,0) + c;
        \\}
        \\print add(1,2,3);
        \\
    ;
    const expected_ouput =
        \\6
        \\
    ;
    try std.testing.expect(try test_program(code, expected_ouput));
}

test "fibonacci recursion" {
    const code =
        \\fun fibonacci(n){
        \\   if (n <= 0){
        \\       return 1;
        \\   } else {
        \\      return fibonacci(n-1) + fibonacci(n-2);
        \\   }
        \\}
        \\for (var i = 0; i < 5; i = i + 1) print fibonacci(i);
    ;
    const expected_ouput =
        \\1
        \\2
        \\3
        \\5
        \\8
        \\
    ;
    try std.testing.expect(try test_program(code, expected_ouput));
}
test "closures" {
    const code =
        \\fun makeCounter() {
        \\  var i = 0;
        \\  fun count() {
        \\    i = i + 1;
        \\    print i;
        \\  }
        \\  return count;
        \\}
        \\var counter = makeCounter();
        \\counter();
        \\counter();
    ;
    const expected_ouput =
        \\1
        \\2
        \\
    ;
    try std.testing.expect(try test_program(code, expected_ouput));
}
