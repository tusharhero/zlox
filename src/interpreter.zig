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
const ast = @import("ast.zig");
const main = @import("main.zig");
const Token = _tokens.Token;
const Type = _tokens.TokenType;

const Object = union(enum) {
    nill: ?void,
    boolean: bool,
    number: f64,
    string: []const u8,
};

pub const Interpreter = struct {
    pub fn init() Interpreter {
        return Interpreter{};
    }
    fn evalUnary(self: *Interpreter, expression: ast.Unary) Object {
        const right = self.evaluate(expression.right);
        return switch (right) {
            .number => |num| Object{ .number = switch (expression.operator._type) {
                Type.MINUS => -1 * num,
                else => num,
            } },
            else => right,
        };
    }
    fn evalBinary(self: *Interpreter, expression: ast.Binary) Object {
        const left = self.evaluate(expression.left);
        const right = self.evaluate(expression.right);
        return switch (expression.operator._type) {
            Type.PLUS => left + right,
            Type.MINUS => left - right,
            Type.STAR => left * right,
            Type.SLASH => left / right,
            else => self.evaluate(expression.right),
        };
    }
    pub fn evaluate(self: *Interpreter, expression: *const ast.Expr) Object {
        return switch (expression.*) {
            .literal => |literal| {
                switch (literal.value.?) {
                    .number => |num| return Object{ .number = num },
                    .string => |str| return Object{ .string = str },
                }
            },
            .grouping => |grouping| self.evaluate(grouping.expression),
            .unary => |unary| self.evalUnary(unary),
            .binary => |binary| self.evalBinary(binary),
        };
    }
};
