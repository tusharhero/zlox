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
const main = @import("main.zig");
const Token = _tokens.Token;
const Type = _tokens.TokenType;

pub const Expr = union(enum) {
    binary: Binary,
    grouping: Grouping,
    literal: Literal,
    unary: Unary,
    variable: Variable,
    assignment: Assignment,
    logical: Binary,
    call: Call,
};

pub const Binary = struct {
    left: *const Expr,
    operator: Token,
    right: *const Expr,
};
pub const Grouping = struct {
    expression: *const Expr,
};
pub const Literal = struct {
    value: ?_tokens.Literal,
};
pub const Unary = struct {
    operator: Token,
    right: *const Expr,
};
pub const Variable = struct {
    name: Token,
};
pub const Assignment = struct {
    name: Token,
    value: *const Expr,
};
pub const Call = struct {
    callee: *const Expr,
    paren: Token,
    arguments: ?std.ArrayList(*const Expr),
};

pub const Stmt = union(enum) {
    expression: *const Expr,
    print: *const Expr,
    variable: VarDecl,
    block: Block,
    _if: IfStmt,
    _while: WhileStmt,
    function: FunDecl,
    class: ClassDecl,
    _return: ReturnStmt,
};
pub const VarDecl = struct {
    name: Token,
    intializer: ?*const Expr,
};
pub const Block = struct {
    statements: std.ArrayList(Stmt),
};
pub const IfStmt = struct {
    condition: *const Expr,
    thenBranch: *const Stmt,
    elseBranch: ?*const Stmt,
};
pub const WhileStmt = struct {
    condition: *const Expr,
    body: *const Stmt,
};
pub const FunDecl = struct {
    name: Token,
    parameters: ?std.ArrayList(Token),
    body: std.ArrayList(Stmt),
};

pub const ClassDecl = struct {
    name: Token,
    methods: std.ArrayList(*const FunDecl),
};

pub const ReturnStmt = struct {
    keyword: Token,
    value: ?*const Expr,
};

pub const Printer = struct {
    arena: std.heap.ArenaAllocator,
    notation: Notation,

    pub const Notation = union(enum) {
        reverse_polish,
        parenthesized_prefix,
    };

    /// Caller must call deinit.
    pub fn init(notation: Notation) !Printer {
        return Printer{
            .arena = std.heap.ArenaAllocator.init(
                std.heap.page_allocator,
            ),
            .notation = notation,
        };
    }

    pub fn deinit(self: *Printer) void {
        self.arena.deinit();
    }

    fn printBinary(self: *Printer, expression: Binary) ![]const u8 {
        return try self.format(.{
            expression.operator.lexeme,
            expression.left,
            expression.right,
        });
    }

    fn printGrouping(self: *Printer, expression: Grouping) ![]const u8 {
        return try self.format(.{ "group", expression.expression });
    }

    fn printLiteral(self: *Printer, expression: Literal) ![]const u8 {
        const literal = expression.value;
        if (literal == null) return "nill";
        return switch (literal.?) {
            .number => |num| try std.fmt.allocPrint(
                self.arena.allocator(),
                "{d}",
                .{num},
            ),
            .string => |str| str,
        };
    }

    fn printUnary(self: *Printer, expression: Unary) ![]const u8 {
        return try self.format(.{
            expression.operator.lexeme,
            expression.right,
        });
    }

    /// args must be an tuple consisting of either []const u8, or Expr.
    fn format(self: *Printer, args: anytype) ![]const u8 {
        return switch (self.notation) {
            .reverse_polish => self.format_reverse_polish(args),
            .parenthesized_prefix => self.format_parethensized_prefix(args),
        };
    }

    /// args must be an tuple consisting of either []const u8, or Expr.
    fn format_parethensized_prefix(self: *Printer, args: anytype) ![]const u8 {
        var formatted_string_list = std.ArrayList(u8).init(self.arena.allocator());
        const writer = formatted_string_list.writer();
        inline for (args, 0..) |arg, index| {
            if (index == 0) {
                try writer.print("({s}", .{arg});
            } else {
                try writer.print(" {!s}", .{self.printExpr(arg)});
            }
        }
        try writer.print(")", .{});
        const formatted_string = formatted_string_list.items;
        return formatted_string;
    }

    /// args must be an tuple consisting of either []const u8, or Expr.
    fn format_reverse_polish(self: *Printer, args: anytype) ![]const u8 {
        var formatted_string_list = std.ArrayList(u8).init(self.arena.allocator());
        const writer = formatted_string_list.writer();
        inline for (args, 0..) |arg, index| {
            if (index != 0) {
                try writer.print("{!s} ", .{self.printExpr(arg)});
            }
        }
        try writer.print("{s}", .{args[0]});
        const formatted_string = formatted_string_list.items;
        return formatted_string;
    }

    pub fn printExpr(self: *Printer, expression: *const Expr) ![]const u8 {
        return switch (expression.*) {
            .binary => |binary| self.printBinary(binary),
            .grouping => |grouping| self.printGrouping(grouping),
            .literal => |literal| self.printLiteral(literal),
            .unary => |unary| self.printUnary(unary),
        };
    }
};
