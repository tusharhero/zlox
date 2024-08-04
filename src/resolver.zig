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
const _tokens = @import("tokens.zig");
const Token = _tokens.Token;
const Type = _tokens.TokenType;
const ast = @import("ast.zig");
const Interpreter = @import("interpreter.zig").Interpreter;
const main = @import("main.zig");

pub fn Resolver(Writer: type) type {
    return struct {
        arena: std.heap.ArenaAllocator,
        interpreter: *Interpreter(Writer),
        scopes: Scopes,

        const Scopes = std.ArrayList(*Scope);
        const Scope = std.StringHashMap(bool);
        const Self = @This();

        /// Caller must call deinit.
        pub fn init(allocator: std.mem.Allocator, interpreter: *Interpreter(Writer)) Self {
            return Self{
                .arena = std.heap.ArenaAllocator.init(allocator),
                .interpreter = interpreter,
                .scopes = Scopes.init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.arena.deinit();
            self.scopes.deinit();
        }

        fn beginScope(self: *Self) !void {
            const allocator = self.arena.allocator();
            const scope = try allocator.create(Scope);
            scope.* = Scope.init(allocator);
            try self.scopes.append(scope);
        }

        fn endScope(self: *Self) void {
            _ = self.scopes.pop();
        }

        fn declare(self: *Self, name: Token) !void {
            if (self.scopes.getLastOrNull()) |scope|
                try scope.put(name.lexeme, false);
        }

        fn define(self: *Self, name: Token) !void {
            if (self.scopes.getLastOrNull()) |scope|
                try scope.put(name.lexeme, true);
        }

        pub fn resolve(self: *Self, resolvee: anytype) main.Errors!void {
            switch (@TypeOf(resolvee)) {
                *const ast.Stmt, *ast.Stmt => try self.resolveStatement(resolvee),
                *const ast.Expr => try self.resolveExpression(resolvee),
                []const *ast.Stmt => for (resolvee) |_statement|
                    try self.resolve(_statement),
                else => |invalid_type| @panic("Invalid type " ++ @typeName(invalid_type) ++ " passed to resolve."),
            }
        }

        fn resolveLocal(self: *Self, expression: *const ast.Expr, name: Token) !void {
            const size = self.scopes.items.len;
            if (size == 0) return;
            var i = size - 1;
            while (i > 0) : (i -= 1) {
                if (self.scopes.items[i].contains(name.lexeme)) {
                    try self.interpreter.resolve(expression, size - 1 - i);
                    break;
                }
            }
        }

        fn resolveStatement(self: *Self, statement: *const ast.Stmt) !void {
            switch (statement.*) {
                .block => |block| {
                    try self.beginScope();
                    for (block.statements.items) |_statement| {
                        try self.resolve(&_statement);
                    }
                    self.endScope();
                },
                .variable => |variable| {
                    try self.declare(variable.name);
                    if (variable.intializer) |initializer|
                        try self.resolve(initializer);
                    try self.define(variable.name);
                },
                .expression => |expression| try self.resolve(expression),
                .print => |print| try self.resolve(print),
                ._if => {},
                .function => |function| {
                    try self.beginScope();
                    try self.declare(function.name);
                    if (function.parameters) |parameters|
                        for (parameters.items) |parameter| {
                            try self.declare(parameter);
                            try self.define(parameter);
                        };
                    for (function.body.items) |_statement|
                        try self.resolve(&_statement);
                    self.endScope();
                },
                ._return => {},
                ._while => {},
            }
        }

        fn resolveExpression(self: *Self, expression: *const ast.Expr) !void {
            switch (expression.*) {
                .variable => |variable| {
                    if (self.scopes.getLastOrNull()) |scope|
                        if (scope.get(variable.name.lexeme)) |defined|
                            if (!defined)
                                try main._error(
                                    .{ .token = variable.name },
                                    "Can't read local variable in its own initializer.",
                                );
                    try self.resolveLocal(expression, variable.name);
                },
                else => {},
            }
        }
    };
}
