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
const Resolver = @import("resolver.zig").Resolver;

fn test_program(source: []const u8) ![]const u8 {
    const test_allocator = std.testing.allocator;

    var output = std.ArrayList(u8).init(test_allocator);
    defer output.deinit();

    const writer = output.writer();

    var lexxer = try Lexxer.init(test_allocator, source);
    defer lexxer.deinit();

    const tokens = try lexxer.scanTokens();

    var parser = try Parser.init(tokens);
    defer parser.deinit();

    var interpreter = try Interpreter(std.ArrayList(u8).Writer).init(
        test_allocator,
        writer,
    );
    defer interpreter.deinit();

    var resolver = Resolver(std.ArrayList(u8).Writer).init(test_allocator, &interpreter);
    defer resolver.deinit();

    const statements = try parser.parse();
    try resolver.resolve(statements);
    try interpreter.interpret(statements);

    return output.toOwnedSlice();
}

fn run_test(code: []const u8, expected_output: []const u8) !void {
    const output = try test_program(code);
    defer std.testing.allocator.free(output);
    const ok = std.mem.eql(u8, output, expected_output);
    if (!ok) {
        std.debug.print(
            "\nexpected:\n{s}\ngot:\n{s}",
            .{ expected_output, output },
        );
        return error.TestUnexpectedResult;
    }
}

test "printing" {
    try run_test(
        \\print 2;
    ,
        \\2
        \\
    );
}

test "var, printing" {
    try run_test(
        \\var a = 1+1;
        \\print a;
    ,
        \\2
        \\
    );
}

test "string concatenation" {
    try run_test(
        \\var zig = "Ziggy";
        \\var lox = "Loxxy";
        \\print zig + lox;
    ,
        \\ZiggyLoxxy
        \\
    );
}

test "scope" {
    try run_test(
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
    ,
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
    );
}

test "if else" {
    try run_test(
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
    ,
        \\Value of a is
        \\1
        \\Value of b is
        \\26
        \\a is smaller than b
        \\
    );
}

test "while loop" {
    try run_test(
        \\var a = 0;
        \\while (a < 10) {
        \\ print a;
        \\ a = a + 1;
        \\}
    ,
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
    );
}

test "for loop" {
    try run_test(
        \\for(var i = 0; i < 10; i = i + 1) {
        \\   print i;
        \\}
    ,
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
    );
}

test "and, or" {
    try run_test(
        \\print 1 and 1;
        \\print true and false;
        \\print false or true;
    ,
        \\1
        \\false
        \\true
        \\
    );
}

test "functions no return" {
    try run_test(
        \\fun hello() {
        \\  print "Hello World!";
        \\}
        \\hello();
        \\
    ,
        \\Hello World!
        \\
    );
}

test "functions return" {
    try run_test(
        \\fun hello() {
        \\  return "Hello World!";
        \\}
        \\print hello();
        \\
    ,
        \\Hello World!
        \\
    );
}

test "simple recursion" {
    try run_test(
        \\fun add(a,b,c) {
        \\  if (c == 0) return a + b;
        \\  return add(a,b,0) + c;
        \\}
        \\print add(1,2,3);
        \\
    ,
        \\6
        \\
    );
}

test "fibonacci recursion" {
    try run_test(
        \\fun fibonacci(n){
        \\   if (n <= 0){
        \\       return 1;
        \\   } else {
        \\      return fibonacci(n-1) + fibonacci(n-2);
        \\   }
        \\}
        \\for (var i = 0; i < 5; i = i + 1) print fibonacci(i);
    ,
        \\1
        \\2
        \\3
        \\5
        \\8
        \\
    );
}
test "closures" {
    try run_test(
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
    ,
        \\1
        \\2
        \\
    );
}

test "lexical scope" {
    try run_test(
        \\var a = "global";
        \\{
        \\    fun whatIsA() {
        \\        print a;
        \\    }
        \\    whatIsA();
        \\    a = "2nd local";
        \\    whatIsA();
        \\    var a = "local";
        \\    whatIsA();
        \\}
    ,
        \\global
        \\2nd local
        \\2nd local
        \\
    );
}

test "nested function returns" {
    try run_test(
        \\fun hello() {
        \\  fun hello2(){
        \\    return 1;
        \\  }
        \\  return 0;
        \\}
    ,
        \\
    );
}

test "class parsing" {
    try run_test(
        \\class DevonshireCream {
        \\  serveOn() {
        \\    return 	  "Scones";
        \\  }
        \\}
        \\print DevonshireCream;
    ,
        \\DevonshireCream
        \\
    );
}

test "class fields" {
    try run_test(
        \\class bagel {}
        \\bagel.name = "bag";
        \\print bagel.name;
    ,
        \\bag
        \\
    );
}
