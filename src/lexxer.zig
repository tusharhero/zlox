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
const tokens = @import("tokens.zig");
const main = @import("main.zig");
const Token = tokens.Token;
const TokenType = tokens.TokenType;

const Lexxer = struct {
    source_code: []u8,
    tokens: std.ArrayList(Token),
    keywords: std.AutoArrayHashMap([]u8, TokenType),

    start: u64,
    current: u64,
    line: u64,

    fn init(self: Lexxer, allocator: std.mem.Allocator, source_code: []u8) Lexxer {
        self.source_code = source_code;
        self.tokens = std.ArrayList(Token).init(allocator);
        errdefer self.tokens.deinit();

        self.keywords = std.AutoArrayHashMap([]u8, TokenType).init(allocator);
        defer self.keywords.deinit();
        const put = self.keywords.put;
        const Type = TokenType;
        put("and", Type.AND);
        put("class", Type.CLASS);
        put("else", Type.ELSE);
        put("false", Type.FALSE);
        put("for", Type.FOR);
        put("fun", Type.FUN);
        put("if", Type.IF);
        put("nil", Type.NIL);
        put("or", Type.OR);
        put("print", Type.PRINT);
        put("return", Type.RETURN);
        put("super", Type.SUPER);
        put("this", Type.THIS);
        put("true", Type.TRUE);
        put("var", Type.VAR);
        put("while", Type.WHILE);

        self.start = 0;
        self.current = 0;
        self.line = 1;
    }

    fn advance(self: Lexxer) u8 {
        self.current += 1;
        return self.source_code[self.current - 1];
    }

    fn addToken_literal(self: Lexxer, _type: TokenType, literal: ?tokens.Literal) void {
        const lexeme: []u8 = self.source_code[self.start..self.current];
        self.tokens.append(Token{ _type, lexeme, literal, self.line });
    }

    fn addToken(self: Lexxer, _type: TokenType) void {
        self.addToken_literal(_type, null);
    }

    fn match(self: Lexxer, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source_code[self.current] != expected) return false;
        self.current += 1;
        return true;
    }

    fn peek(self: Lexxer) u8 {
        if (self.isAtEnd()) return 0;
        return self.source_code[self.current];
    }

    fn peekNext(self: Lexxer) u8 {
        if (self.current + 1 >= self.source_code.length()) return 0;
        return self.source_code[self.current + 1];
    }

    fn string(self: Lexxer) void {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            self.advance();
        }

        if (self.isAtEnd()) {
            main._error(self.line, "Unterminated String.");
        }

        // The closing ".
        self.advance();

        const value: []u8 = self.source_code[self.start + 1 .. self.current - 1];
        self.addToken(TokenType.STRING, value);
    }

    fn number(self: Lexxer) void {
        const isDigit = std.ascii.isDigit;
        while (isDigit(self.peek())) self.advance();

        // Look for fractional part.
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            self.advance();
            while (isDigit(self.peek())) self.advance();
        }

        addToken(
            TokenType.NUMBER,
            std.fmt.parseFloat(
                f64,
                self.source_code[self.start..self.current],
            ),
        );
    }

    fn identifier(self: Lexxer) void {
        const isAlphanumeric = std.ascii.isAlphanumeric;
        while (isAlphanumeric(self.peek()) or self.peek() == '_') self.advance();
        const text = self.source_code[self.start..self.current];
        const _type = self.keywords.get(text).?;
        self.addToken(_type);
    }

    fn scanToken(self: Lexxer) void {
        const c = self.advance();
        const Type = TokenType;
        switch (c) {
            '(' => self.addToken(Type.LEFT_PAREN),
            ')' => self.addToken(Type.RIGHT_PAREN),
            '{' => self.addToken(Type.LEFT_BRACE),
            '}' => self.addToken(Type.RIGHT_BRACE),
            ',' => self.addToken(Type.COMMA),
            '.' => self.addToken(Type.DOT),
            '-' => self.addToken(Type.MINUS),
            '+' => self.addToken(Type.PLUS),
            ';' => self.addToken(Type.SEMICOLON),
            '*' => self.addToken(Type.STAR),

            '!' => self.addToken(if (self.match('=')) Type.EQUAL else Type.BANG),
            '=' => self.addToken(if (self.match('=')) Type.EQUAL_EQUAL else Type.EQUAL),
            '<' => self.addToken(if (self.match('=')) Type.LESS_EQUAL else Type.LESS),
            '>' => self.addToken(if (self.match('=')) Type.GREATER_EQUAL else Type.GREATER),

            '/' => {
                if (self.match('/')) { // Comment
                    while ((self.peek() != '\n' and !self.isAtEnd())) self.advance();
                } else {
                    addToken(Type.SLASH);
                }
            },

            ' ', '\r', '\t' => {},

            '\n' => self.line += 1,

            '"' => self.string(),

            '0'...'9' => self.number(),

            'a'...'z', 'A'...'Z', '_' => self.identifier(),

            else => main._error(self.line, "Unexpected character."),
        }
    }

    fn isAtEnd(self: Lexxer) bool {
        return self.current >= self.source_code.len;
    }

    fn scanTokens(self: Lexxer) std.ArrayList(Token) {
        while (!self.isAtEnd()) {
            // We are at the beginning of the next lexeme.
            self.start = self.current;
            self.scanToken();
        }
        tokens.add(Token{ TokenType.EOF, "", null, self.line });
        return self.tokens;
    }
};
