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

pub const Lexxer = struct {
    source_code: []const u8,
    tokens: std.ArrayList(Token),

    start: u64 = 0,
    current: u64 = 0,
    line: u64 = 1,

    const keywords = std.StaticStringMap(Type).initComptime(.{
        .{ "and", .AND },
        .{ "class", .CLASS },
        .{ "else", .ELSE },
        .{ "false", .FALSE },
        .{ "for", .FOR },
        .{ "fun", .FUN },
        .{ "if", .IF },
        .{ "nil", .NIL },
        .{ "or", .OR },
        .{ "print", .PRINT },
        .{ "return", .RETURN },
        .{ "super", .SUPER },
        .{ "this", .THIS },
        .{ "true", .TRUE },
        .{ "var", .VAR },
        .{ "while", .WHILE },
    });

    /// Caller must call deinit.
    pub fn init(allocator: std.mem.Allocator, source_code: []const u8) !Lexxer {
        const tokens = std.ArrayList(Token).init(allocator);
        return Lexxer{ .source_code = source_code, .tokens = tokens };
    }

    pub fn deinit(self: *Lexxer) void {
        self.tokens.deinit();
    }

    fn advance(self: *Lexxer) u8 {
        self.current += 1;
        return self.source_code[self.current - 1];
    }

    fn addToken(self: *Lexxer, _type: Type, literal: ?_tokens.Literal) !void {
        const lexeme: []const u8 = self.source_code[self.start..self.current];
        try self.tokens.append(Token{
            ._type = _type,
            .lexeme = lexeme,
            .literal = literal,
            .line = self.line,
        });
    }

    fn match(self: *Lexxer, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source_code[self.current] != expected) return false;
        self.current += 1;
        return true;
    }

    fn peek(self: *Lexxer) u8 {
        if (self.isAtEnd()) return 0;
        return self.source_code[self.current];
    }

    fn peekNext(self: *Lexxer) u8 {
        if (self.current + 1 >= self.source_code.len) return 0;
        return self.source_code[self.current + 1];
    }

    fn string(self: *Lexxer) !void {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            try main._error(
                .{ .line = self.line },
                "Unterminated String.",
            );
        }

        // The closing ".
        _ = self.advance();

        const value: []const u8 = self.source_code[self.start + 1 .. self.current - 1];
        try self.addToken(Type.STRING, _tokens.Literal{ .string = value });
    }

    fn number(self: *Lexxer) !void {
        const isDigit = std.ascii.isDigit;
        while (isDigit(self.peek())) _ = self.advance();

        // Look for fractional part.
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance();
            while (isDigit(self.peek())) _ = self.advance();
        }

        try self.addToken(
            Type.NUMBER,
            _tokens.Literal{ .number = try std.fmt.parseFloat(
                f64,
                self.source_code[self.start..self.current],
            ) },
        );
    }

    fn identifier(self: *Lexxer) !void {
        const isAlphanumeric = std.ascii.isAlphanumeric;
        while (isAlphanumeric(self.peek()) or self.peek() == '_') _ = self.advance();

        const text = self.source_code[self.start..self.current];
        const _type = keywords.get(text) orelse Type.IDENTIFIER;
        switch (_type) {
            Type.TRUE => try self.addToken(_type, _tokens.Literal{ .boolean = true }),
            Type.FALSE => try self.addToken(_type, _tokens.Literal{ .boolean = false }),
            else => try self.addToken(_type, _tokens.Literal{ .string = text }),
        }
    }

    fn comments(self: *Lexxer) !void {
        switch (self.peek()) {
            '/' => { // Single line comments
                while ((self.peek() != '\n' and !self.isAtEnd())) {
                    _ = self.advance();
                }
            },
            '*' => { // C-style block comments
                _ = self.advance();
                var nested_level: u64 = 1;
                while (nested_level != 0) {
                    if (self.match('/') and self.match('*')) nested_level += 1;
                    if (self.match('*') and self.match('/')) nested_level -= 1;

                    if (self.isAtEnd()) try main._error(
                        .{ .line = self.line },
                        "Unterminated String.",
                    );
                    if (self.advance() == '\n') self.line += 1;
                }
            },
            else => try self.addToken(Type.SLASH, null),
        }
    }

    fn scanToken(self: *Lexxer) !void {
        const c = self.advance();
        try switch (c) {
            '(' => self.addToken(Type.LEFT_PAREN, null),
            ')' => self.addToken(Type.RIGHT_PAREN, null),
            '{' => self.addToken(Type.LEFT_BRACE, null),
            '}' => self.addToken(Type.RIGHT_BRACE, null),
            ',' => self.addToken(Type.COMMA, null),
            '.' => self.addToken(Type.DOT, null),
            '-' => self.addToken(Type.MINUS, null),
            '+' => self.addToken(Type.PLUS, null),
            ';' => self.addToken(Type.SEMICOLON, null),
            '*' => self.addToken(Type.STAR, null),

            '!' => self.addToken(if (self.match('=')) Type.BANG_EQUAL else Type.BANG, null),
            '=' => self.addToken(if (self.match('=')) Type.EQUAL_EQUAL else Type.EQUAL, null),
            '<' => self.addToken(if (self.match('=')) Type.LESS_EQUAL else Type.LESS, null),
            '>' => self.addToken(if (self.match('=')) Type.GREATER_EQUAL else Type.GREATER, null),

            '/' => self.comments(),

            ' ', '\r', '\t' => {},

            '\n' => self.line += 1,

            '"' => self.string(),

            '0'...'9' => self.number(),

            'a'...'z', 'A'...'Z', '_' => self.identifier(),

            else => main._error(.{ .line = self.line }, "Unexpected character."),
        };
    }

    fn isAtEnd(self: *Lexxer) bool {
        return self.current >= self.source_code.len;
    }

    pub fn scanTokens(self: *Lexxer) ![]const Token {
        while (!self.isAtEnd()) {
            // We are at the beginning of the next lexeme.
            self.start = self.current;
            try self.scanToken();
        }
        // Add EOF token at the end.
        try self.tokens.append(Token{
            ._type = Type.EOF,
            .lexeme = "",
            .literal = null,
            .line = self.line,
        });
        return self.tokens.items;
    }
};
