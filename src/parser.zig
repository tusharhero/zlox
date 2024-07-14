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

pub const Parser = struct {
    arena: std.heap.ArenaAllocator,
    tokens: std.ArrayList(Token),
    current: u64,

    const Error = error{
        ParseError,
    };

    /// Caller must call deinit.
    pub fn init(tokens: std.ArrayList(Token)) !Parser {
        return Parser{
            .arena = std.heap.ArenaAllocator.init(
                std.heap.page_allocator,
            ),
            .tokens = tokens,
            .current = 0,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.arena.deinit();
    }

    fn peek(self: *Parser) Token {
        return self.tokens.items[self.current];
    }

    fn previous(self: *Parser) Token {
        return self.tokens.items[self.current - 1];
    }

    fn isAtEnd(self: *Parser) bool {
        return self.peek()._type == Type.EOF;
    }

    fn advance(self: *Parser) Token {
        if (!self.isAtEnd()) self.current += 1;
        return self.previous();
    }

    fn check(self: *Parser, __type: Type) bool {
        if (self.isAtEnd()) return false;
        return self.peek()._type == __type;
    }

    fn match(self: *Parser, types: anytype) bool {
        inline for (types) |__type| {
            if (@TypeOf(__type) != Type) unreachable;
            if (self.check(__type)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn _error(self: *Parser, token: Token, message: []const u8) !void {
        _ = self;
        try main._error(.{ .token = token }, message);
    }

    fn consume(self: *Parser, _type: Type, message: []const u8) !Token {
        if (self.check(_type)) return self.advance();
        try self._error(self.peek(), message);
        return error.ParseError;
    }

    const errors = error{
        ParseError,
        DiskQuota,
        FileTooBig,
        InputOutput,
        NoSpaceLeft,
        DeviceBusy,
        InvalidArgument,
        AccessDenied,
        BrokenPipe,
        SystemResources,
        OperationAborted,
        NotOpenForWriting,
        LockViolation,
        WouldBlock,
        ConnectionResetByPeer,
        OutOfMemory,
        Unexpected,
    };

    pub fn parse(self: *Parser) errors!*ast.Expr {
        return self.expression();
    }

    fn expression(self: *Parser) errors!*ast.Expr {
        return try self.comma();
    }

    fn comma(self: *Parser) !*ast.Expr {
        var expr = try self.equality();
        while (self.match(.{Type.COMMA})) {
            const operator: Token = self.previous();
            const compound_expr = try self.arena.allocator().create(ast.Expr);
            const right = try self.equality();
            compound_expr.* = ast.Expr{ .binary = ast.Binary{
                .left = expr,
                .operator = operator,
                .right = right,
            } };
            expr = compound_expr;
        }
        return expr;
    }

    fn equality(self: *Parser) !*ast.Expr {
        var expr = try self.comparison();
        while (self.match(.{ Type.BANG_EQUAL, Type.EQUAL_EQUAL })) {
            const operator = self.previous();
            const compound_expr = try self.arena.allocator().create(ast.Expr);
            const right = try self.comparison();
            compound_expr.* = ast.Expr{ .binary = ast.Binary{
                .left = expr,
                .operator = operator,
                .right = right,
            } };
            expr = compound_expr;
        }
        return expr;
    }

    fn comparison(self: *Parser) !*ast.Expr {
        var expr = try self.term();
        while (self.match(.{
            Type.LESS,    Type.LESS_EQUAL,
            Type.GREATER, Type.GREATER_EQUAL,
        })) {
            const operator: Token = self.previous();
            const compound_expr = try self.arena.allocator().create(ast.Expr);
            const right = try self.term();
            compound_expr.* = ast.Expr{ .binary = ast.Binary{
                .left = expr,
                .operator = operator,
                .right = right,
            } };
            expr = compound_expr;
        }
        return expr;
    }

    fn term(self: *Parser) !*ast.Expr {
        var expr = try self.factor();
        while (self.match(.{ Type.PLUS, Type.MINUS })) {
            const operator: Token = self.previous();
            const compound_expr = try self.arena.allocator().create(ast.Expr);
            const right = try self.factor();
            compound_expr.* = ast.Expr{ .binary = ast.Binary{
                .left = expr,
                .operator = operator,
                .right = right,
            } };
            expr = compound_expr;
        }
        return expr;
    }

    fn factor(self: *Parser) !*ast.Expr {
        var expr = try self.unary();
        while (self.match(.{ Type.STAR, Type.SLASH })) {
            const operator: Token = self.previous();
            const compound_expr = try self.arena.allocator().create(ast.Expr);
            const right = try self.unary();
            compound_expr.* = ast.Expr{ .binary = ast.Binary{
                .left = expr,
                .operator = operator,
                .right = right,
            } };
            expr = compound_expr;
        }
        return expr;
    }

    fn unary(self: *Parser) !*ast.Expr {
        const expr = try self.arena.allocator().create(ast.Expr);
        while (self.match(.{ Type.BANG, Type.MINUS })) {
            const operator: Token = self.previous();
            const right = try self.unary();
            expr.* = ast.Expr{ .unary = ast.Unary{
                .operator = operator,
                .right = right,
            } };
            return expr;
        }
        return self.primary();
    }

    fn primary(self: *Parser) !*ast.Expr {
        var expr = try self.arena.allocator().create(ast.Expr);
        if (self.match(.{Type.FALSE})) {
            expr.* = ast.Expr{
                .literal = ast.Literal{
                    .value = _tokens.Literal{
                        .string = "false",
                    },
                },
            };
            return expr;
        }
        if (self.match(.{Type.TRUE})) {
            expr.* = ast.Expr{
                .literal = ast.Literal{
                    .value = _tokens.Literal{
                        .string = "true",
                    },
                },
            };
            return expr;
        }
        if (self.match(.{Type.NIL})) {
            expr.* = ast.Expr{
                .literal = ast.Literal{
                    .value = null,
                },
            };
            return expr;
        }
        if (self.match(.{ Type.NUMBER, Type.STRING })) {
            expr.* = ast.Expr{
                .literal = ast.Literal{
                    .value = self.previous().literal,
                },
            };
            return expr;
        }
        if (self.match(.{Type.LEFT_PAREN})) {
            expr = try self.expression();
            _ = try self.consume(
                Type.RIGHT_PAREN,
                "Expect ')' after expression.",
            );
            const compound_expr = try self.arena.allocator().create(ast.Expr);
            compound_expr.* = ast.Expr{
                .grouping = ast.Grouping{
                    .expression = expr,
                },
            };
            expr = compound_expr;
            return expr;
        }

        try self._error(self.peek(), "Expect expression.");
        return error.ParseError;
    }
};
