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

fn _error(token: Token, message: []const u8) !void {
    try main._error(
        .{ .token = token },
        message,
    );
}
const Error = error{RuntimeError};
const Errors = Error || main.Errors;

pub fn Interpreter(Writer: type) type {
    return struct {
        arena: std.heap.ArenaAllocator,
        manual_allocator: std.mem.Allocator,
        environment: *Env,
        global: *Env,
        writer: Writer,
        locals: ExprIntHashMap,

        const ExprIntHashMap = std.HashMap(
            *const ast.Expr,
            u64,
            struct {
                const Context = @This();
                pub fn hash(context: Context, expression: *const ast.Expr) u64 {
                    var h = std.hash.Wyhash.init(0);
                    h.update(std.mem.asBytes(&@intFromEnum(expression.*)));
                    switch (expression.*) {
                        .unary => |unary| {
                            h.update(std.mem.asBytes(&@intFromEnum(unary.operator._type)));
                            h.update(std.mem.asBytes(&hash(context, unary.right)));
                        },
                        .binary, .logical => |binary| {
                            h.update(std.mem.asBytes(&@intFromEnum(binary.operator._type)));
                            h.update(std.mem.asBytes(&hash(context, binary.left)));
                            h.update(std.mem.asBytes(&hash(context, binary.right)));
                        },
                        .grouping => |grouping| {
                            h.update(std.mem.asBytes(&hash(context, grouping.expression)));
                        },
                        .literal => |literal| {
                            if (literal.value) |lit| {
                                h.update(std.mem.asBytes(&@intFromEnum(lit)));
                                switch (lit) {
                                    .string => |str| h.update(str),
                                    .boolean => |boolean| h.update(std.mem.asBytes(&@intFromBool(boolean))),
                                    .number => |num| h.update(std.mem.asBytes(&num)),
                                }
                            }
                        },
                        .variable => |variable| h.update(std.mem.asBytes(&variable)),
                        .assignment => |assignment| {
                            h.update(std.mem.asBytes(&assignment.name.lexeme));
                            h.update(std.mem.asBytes(&hash(context, assignment.value)));
                        },
                        .call => |call| {
                            h.update(std.mem.asBytes(&@intFromEnum(call.paren._type)));
                            h.update(std.mem.asBytes(&hash(context, call.callee)));
                            if (call.arguments) |arguments|
                                for (arguments.items) |argument|
                                    h.update(std.mem.asBytes(&hash(context, argument)));
                        },
                        .get => |get| {
                            h.update(std.mem.asBytes(&hash(context, get.object)));
                            h.update(std.mem.asBytes(&get.name));
                        },
                        .set => |set| {
                            h.update(std.mem.asBytes(&hash(context, set.object)));
                            h.update(std.mem.asBytes(&hash(context, set.value)));
                            h.update(std.mem.asBytes(&set.name));
                        },
                    }
                    return h.final();
                }
                pub fn eql(context: Context, a: *const ast.Expr, b: *const ast.Expr) bool {
                    if (@intFromEnum(a.*) != @intFromEnum(b.*)) return false;
                    switch (a.*) {
                        .binary => {
                            if (a.binary.operator._type != a.binary.operator._type) return false;
                            if (!eql(context, a.binary.left, b.binary.left)) return false;
                            if (!eql(context, a.binary.right, b.binary.right)) return false;
                        },
                        .logical => {
                            if (a.logical.operator._type != a.logical.operator._type) return false;
                            if (!eql(context, a.logical.left, b.logical.left)) return false;
                            if (!eql(context, a.logical.right, b.logical.right)) return false;
                        },
                        .grouping => {
                            if (!eql(context, a.grouping.expression, b.grouping.expression)) return false;
                        },
                        .literal => {
                            if (a.literal.value != null and b.literal.value != null) {
                                if (@intFromEnum(a.literal.value.?) != @intFromEnum(b.literal.value.?)) return false;
                                switch (a.literal.value.?) {
                                    .number => if (a.literal.value.?.number != b.literal.value.?.number) return false,
                                    .boolean => if (a.literal.value.?.boolean != b.literal.value.?.boolean) return false,
                                    .string => if (!std.mem.eql(u8, a.literal.value.?.string, b.literal.value.?.string))
                                        return false,
                                }
                            }
                        },
                        .unary => {
                            if (a.unary.operator._type != a.unary.operator._type) return false;
                            if (!eql(context, a.unary.right, b.unary.right)) return false;
                        },
                        .variable => {
                            if (!std.mem.eql(u8, a.variable.name.lexeme, b.variable.name.lexeme)) return false;
                            if (a.variable.name.line != b.variable.name.line) return false;
                            if (a.variable.name.line != b.variable.name.line) return false;
                            if (a.variable.name.literal != null and b.variable.name.literal != null) {
                                if (@intFromEnum(a.variable.name.literal.?) != @intFromEnum(b.variable.name.literal.?)) return false;
                                switch (a.variable.name.literal.?) {
                                    .number => if (a.variable.name.literal.?.number != b.variable.name.literal.?.number) return false,
                                    .boolean => if (a.variable.name.literal.?.boolean != b.variable.name.literal.?.boolean) return false,
                                    .string => if (!std.mem.eql(u8, a.variable.name.literal.?.string, b.variable.name.literal.?.string))
                                        return false,
                                }
                            }
                            if (!std.mem.eql(u8, std.mem.asBytes(&a.variable), std.mem.asBytes(&b.variable))) return false;
                        },
                        .assignment => {
                            if (!std.mem.eql(u8, a.assignment.name.lexeme, b.assignment.name.lexeme)) return false;
                            if (!eql(context, a.assignment.value, b.assignment.value)) return false;
                        },
                        .call => {
                            if (!eql(context, a.call.callee, b.call.callee)) return false;
                            if (a.call.paren.line != a.call.paren.line) return false;
                            if (a.call.arguments != null and b.call.arguments != null) {
                                for (a.call.arguments.?.items, b.call.arguments.?.items) |argumentA, argumentB| {
                                    if (!eql(context, argumentA, argumentB)) return false;
                                }
                            }
                        },
                        .get => {
                            if (!eql(context, a.get.object, b.get.object)) return false;
                            if (!std.mem.eql(u8, a.get.name.lexeme, b.get.name.lexeme)) return false;
                            if (a.get.name.line != b.get.name.line) return false;
                            if (a.get.name.line != b.get.name.line) return false;
                            if (a.get.name.literal != null and b.get.name.literal != null) {
                                if (@intFromEnum(a.get.name.literal.?) != @intFromEnum(b.get.name.literal.?)) return false;
                                switch (a.get.name.literal.?) {
                                    .number => if (a.get.name.literal.?.number != b.get.name.literal.?.number) return false,
                                    .boolean => if (a.get.name.literal.?.boolean != b.get.name.literal.?.boolean) return false,
                                    .string => if (!std.mem.eql(u8, a.get.name.literal.?.string, b.get.name.literal.?.string))
                                        return false,
                                }
                            }
                            if (!std.mem.eql(u8, std.mem.asBytes(&a.get), std.mem.asBytes(&b.get))) return false;
                        },
                        .set => {
                            if (!eql(context, a.set.object, b.set.object)) return false;
                            if (!std.mem.eql(u8, a.set.name.lexeme, b.set.name.lexeme)) return false;
                            if (a.set.name.line != b.set.name.line) return false;
                            if (a.set.name.line != b.set.name.line) return false;
                            if (a.set.name.literal != null and b.set.name.literal != null) {
                                if (@intFromEnum(a.set.name.literal.?) != @intFromEnum(b.set.name.literal.?)) return false;
                                switch (a.set.name.literal.?) {
                                    .number => if (a.set.name.literal.?.number != b.set.name.literal.?.number) return false,
                                    .boolean => if (a.set.name.literal.?.boolean != b.set.name.literal.?.boolean) return false,
                                    .string => if (!std.mem.eql(u8, a.set.name.literal.?.string, b.set.name.literal.?.string))
                                        return false,
                                }
                            }
                            if (!std.mem.eql(u8, std.mem.asBytes(&a.set), std.mem.asBytes(&b.set))) return false;
                        },
                    }
                    return true;
                }
            },
            std.hash_map.default_max_load_percentage,
        );

        const Self = @This();

        const Object = union(enum) {
            nil,
            boolean: bool,
            number: f64,
            string: []const u8,
            callable: Callable,
        };

        pub const Env = struct {
            values: std.StringArrayHashMap(Object),
            enclosing_environment: ?*Env,
            arena: std.heap.ArenaAllocator,

            /// Caller must call deinit.
            pub fn init(allocator: std.mem.Allocator, enclosing_environment: ?*Env) Env {
                return Env{
                    .values = std
                        .StringArrayHashMap(Object).init(allocator),
                    .enclosing_environment = enclosing_environment,
                    .arena = std.heap.ArenaAllocator.init(
                        std.heap.page_allocator,
                    ),
                };
            }
            pub fn deinit(self: *Env) void {
                self.arena.deinit();
                self.values.deinit();
            }

            pub fn define(self: *Env, name: []const u8, value: Object) !void {
                // Duplicate the name because its string maybe freed.
                const name_dupe = try self.arena.allocator().dupe(u8, name);
                try self.values.put(name_dupe, value);
            }

            fn ancestor(self: *Env, distance: u64) *Env {
                var env = self;
                var i: u64 = 0;
                while (i < distance) : (i += 1) {
                    if (env.enclosing_environment) |enclosing_environment|
                        env = enclosing_environment
                    else
                        break;
                }
                return env;
            }

            pub fn assign(self: *Env, name: Token, value: Object) !void {
                if (self.values.contains(name.lexeme)) {
                    // Duplicate the name because its string maybe freed.
                    const name_dupe = try self.arena.allocator().dupe(u8, name.lexeme);
                    try self.values.put(name_dupe, value);
                } else {
                    if (self.enclosing_environment) |enclosing_environment| {
                        try enclosing_environment.assign(name, value);
                    } else {
                        try _error(name, "Undefined variable.");
                        return;
                    }
                }
            }

            pub fn assignAt(self: *Env, distance: u64, name: Token, value: Object) !void {
                try self.ancestor(distance).assign(name, value);
            }

            pub fn get(self: *Env, name: Token) !Object {
                if (self.values.contains(name.lexeme))
                    return self.values.get(name.lexeme).?;
                if (self.enclosing_environment) |enclosing_environment| {
                    return enclosing_environment.get(name);
                }
                try _error(name, "Undefined variable.");

                return Error.RuntimeError;
            }

            pub fn getAt(self: *Env, distance: u64, name: Token) !Object {
                return try self.ancestor(distance).get(name);
            }

            pub fn debugPrint(self: *Env) !void {
                var it = self.values.iterator();
                var i: u64 = 0;
                while (it.next()) |pair| : (i += 1) {
                    std.debug.print(
                        "{d}: {s}: {any}\n",
                        .{ i, pair.key_ptr.*, pair.value_ptr },
                    );
                }
            }
        };

        pub const Callable = struct {
            data: *anyopaque,
            arity: *const fn (data: *anyopaque) u8,
            call: *const fn (data: *anyopaque, interpreter: *Self, arguments: ?std.ArrayList(Object)) Errors!Object,
            toString: *const fn (data: *anyopaque) Errors![]const u8,
        };

        const Clock = struct {
            pub fn init() Callable {
                return Callable{
                    .data = undefined,
                    .arity = arity,
                    .call = call,
                    .toString = toString,
                };
            }
            fn arity(_: *anyopaque) u8 {
                return 0;
            }
            fn call(
                _: *anyopaque,
                interpreter: *Self,
                arguments: ?std.ArrayList(Object),
            ) Errors!Object {
                _ = interpreter;
                _ = arguments;
                return .{
                    .number = @floatFromInt(std.time.timestamp()),
                };
            }
            fn toString(_: *anyopaque) ![]const u8 {
                return "<native fn: clock>";
            }
        };

        const Function = struct {
            allocator: std.mem.Allocator,
            declaration: ast.FunDecl,
            closure: *Env,
            pub fn init(self: *Function) Callable {
                return Callable{
                    .data = self,
                    .arity = arity,
                    .call = call,
                    .toString = toString,
                };
            }
            fn arity(data: *anyopaque) u8 {
                const self: *Function = @ptrCast(@alignCast(data));
                const parameters = self.declaration.parameters;
                return if (parameters != null)
                    @intCast(parameters.?.items.len)
                else
                    0;
            }
            fn call(
                data: *anyopaque,
                interpreter: *Self,
                arguments: ?std.ArrayList(Object),
            ) Errors!Object {
                const self: *Function = @ptrCast(@alignCast(data));
                const declaration = self.declaration;
                const allocator = self.allocator;

                const call_env = interpreter.environment;
                const closure_env = self.closure;
                const child_env = try allocator.create(Env);
                child_env.* = Env.init(allocator, closure_env);
                interpreter.environment = child_env;
                defer interpreter.environment = call_env;

                if (arguments != null and declaration.parameters != null)
                    for (arguments.?.items, declaration.parameters.?.items) |argument, parameter| {
                        try interpreter.environment.define(parameter.lexeme, argument);
                    };

                for (declaration.body.items) |statement| {
                    const return_value = try interpreter.execute(&statement);
                    if (return_value) |val| return val;
                }

                return Object.nil;
            }
            fn toString(data: *anyopaque) ![]const u8 {
                const self: *Function = @ptrCast(@alignCast(data));
                var string_list = std.ArrayList(u8)
                    .init(self.allocator);
                try string_list.writer().print(
                    "<userdefined fn: {s}>",
                    .{self.declaration.name.lexeme},
                );
                const string = string_list.items;
                return string;
            }
        };

        const Class = struct {
            allocator: std.mem.Allocator,
            name: []const u8,
            pub fn init(self: *Class) Callable {
                return Callable{
                    .data = self,
                    .arity = arity,
                    .call = call,
                    .toString = toString,
                };
            }
            fn arity(_: *anyopaque) u8 {
                return 0;
            }
            fn call(
                data: *anyopaque,
                interpreter: *Self,
                arguments: ?std.ArrayList(Object),
            ) Errors!Object {
                _ = interpreter;
                _ = arguments;
                const self: *Class = @ptrCast(@alignCast(data));
                const instance = try self.allocator.create(Instance);
                instance.* = Instance{
                    .allocator = self.allocator,
                    .class = self,
                    .fields = std.StringHashMap(Object).init(self.allocator),
                };
                const callable = instance.init();
                return .{ .callable = callable };
            }
            fn toString(data: *anyopaque) ![]const u8 {
                const self: *Class = @ptrCast(@alignCast(data));
                return self.name;
            }
        };

        const Instance = struct {
            allocator: std.mem.Allocator,
            class: *const Class,
            fields: std.StringHashMap(Object),

            pub fn init(self: *Instance) Callable {
                return Callable{
                    .data = self,
                    .arity = arity,
                    .call = call,
                    .toString = toString,
                };
            }
            fn arity(_: *anyopaque) u8 {
                return 0;
            }
            fn call(
                data: *anyopaque,
                interpreter: *Self,
                arguments: ?std.ArrayList(Object),
            ) Errors!Object {
                _ = data;
                _ = interpreter;
                _ = arguments;
                return Object.nil;
            }
            fn toString(data: *anyopaque) ![]const u8 {
                const self: *Instance = @ptrCast(@alignCast(data));
                var string_list = std.ArrayList(u8)
                    .init(self.allocator);
                try string_list.writer().print("{s} instance", .{self.class.name});
                const string = string_list.items;
                return string;
            }
            fn get(data: *anyopaque, name: Token) !Object {
                const self: *Instance = @ptrCast(@alignCast(data));
                if (self.fields.contains(name.lexeme))
                    return self.fields.get(name.lexeme)
                else
                    try _error(name, "Undefined property.");
            }
            fn set(data: *anyopaque, name: Token, value: Object) !Object {
                const self: *Instance = @ptrCast(@alignCast(data));
                try self.fields.put(name.lexeme, value);
            }
        };

        /// Caller must call deinit.
        pub fn init(allocator: std.mem.Allocator, writer: Writer) !Self {
            const global = try allocator.create(Env);
            global.* = Env.init(allocator, null);
            try global.define(
                "clock",
                .{
                    .callable = Clock.init(),
                },
            );

            const environment = global;

            return .{
                .arena = std.heap.ArenaAllocator.init(
                    std.heap.page_allocator,
                ),
                .manual_allocator = allocator,
                .environment = environment,
                .global = global,
                .writer = writer,
                .locals = ExprIntHashMap.init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.global.deinit();
            self.locals.deinit();
            self.manual_allocator.destroy(self.global);
            self.arena.deinit();
        }

        fn truthVal(self: *Self, object: Object) bool {
            _ = self;
            return switch (object) {
                .boolean => |boolean| return boolean,
                .nil => return false,
                else => true,
            };
        }

        fn evalUnary(self: *Self, expression: ast.Unary) !Object {
            const right = try self.evaluate(expression.right);
            const operator = expression.operator._type;
            switch (operator) {
                Type.MINUS => {
                    switch (right) {
                        .number => |num| return Object{ .number = -1 * num },
                        else => {
                            try _error(
                                expression.operator,
                                "Operand must be a number.",
                            );
                            return Error.RuntimeError;
                        },
                    }
                },
                Type.BANG => return Object{ .boolean = !self.truthVal(right) },
                else => {
                    try _error(
                        expression.operator,
                        "is not an unary operator.",
                    );
                    return Error.RuntimeError;
                },
            }
            return Error.RuntimeError;
        }

        fn evalBinary(self: *Self, expression: ast.Binary) !Object {
            const left = try self.evaluate(expression.left);
            const right = try self.evaluate(expression.right);
            if (@intFromEnum(left) != @intFromEnum(right)) {
                try _error(
                    expression.operator,
                    "Binary operations between different types are not supported.",
                );
                return Error.RuntimeError;
            }
            switch (left) {
                .number => {
                    const l = left.number;
                    const r = right.number;
                    return switch (expression.operator._type) {
                        Type.PLUS => Object{ .number = l + r },
                        Type.MINUS => Object{ .number = l - r },
                        Type.STAR => Object{ .number = l * r },
                        Type.SLASH => Object{ .number = l / r },

                        // Not reversing left and right operands gives incorrect results.
                        Type.GREATER => Object{ .boolean = r < l },
                        Type.GREATER_EQUAL => Object{ .boolean = r <= l },
                        Type.LESS => Object{ .boolean = r > l },
                        Type.LESS_EQUAL => Object{ .boolean = r >= l },
                        Type.EQUAL_EQUAL => Object{ .boolean = r == l },
                        Type.BANG_EQUAL => Object{ .boolean = r != l },

                        else => {
                            try _error(
                                expression.operator,
                                "is not an binary operator for numbers.",
                            );
                            return Error.RuntimeError;
                        },
                    };
                },
                .string => {
                    const l = left.string;
                    const r = right.string;
                    return switch (expression.operator._type) {
                        Type.PLUS => Object{
                            .string = try std.fmt.allocPrint(
                                self.arena.allocator(),
                                "{s}{s}",
                                .{ l, r },
                            ),
                        },
                        Type.EQUAL_EQUAL => Object{ .boolean = std.mem.eql(u8, r, l) },
                        Type.BANG_EQUAL => Object{ .boolean = !std.mem.eql(u8, r, l) },
                        else => {
                            try _error(
                                expression.operator,
                                "is not an binary operator for strings.",
                            );
                            return Error.RuntimeError;
                        },
                    };
                },
                else => {
                    try _error(
                        expression.operator,
                        "Binary operations are not support for type.",
                    );
                    return Error.RuntimeError;
                },
            }
            return Error.RuntimeError;
        }

        fn evalAssignment(self: *Self, expression: *const ast.Expr) !Object {
            const value = try self.evaluate(expression.assignment.value);
            if (self.locals.get(expression)) |distance|
                try self.environment.assignAt(distance, expression.assignment.name, value)
            else
                try self.global.assign(expression.assignment.name, value);
            return value;
        }

        fn evalLogical(self: *Self, expression: ast.Binary) !Object {
            const left = try self.evaluate(expression.left);
            switch (expression.operator._type) {
                Type.OR => if (self.truthVal(left)) return left,
                Type.AND => if (!self.truthVal(left)) return left,
                else => unreachable,
            }
            return self.evaluate(expression.right);
        }

        fn evalCall(self: *Self, expression: ast.Call) !Object {
            const callee = try self.evaluate(expression.callee);
            switch (callee) {
                .callable => {},
                else => {
                    try _error(expression.paren, "Only call functions and classes.");
                    return Error.RuntimeError;
                },
            }
            var arguments: ?std.ArrayList(Object) = null;
            const no_of_args = if (expression.arguments != null)
                expression.arguments.?.items.len
            else
                0;
            if (expression.arguments != null) {
                arguments = std.ArrayList(Object).init(self.arena.allocator());
                for (expression.arguments.?.items) |argument| {
                    try arguments.?.append(try self.evaluate(argument));
                }
            }
            const function: Object = callee;
            const data = function.callable.data;
            if (no_of_args != function.callable.arity(data)) {
                try _error(expression.paren, "Incorrect number of arguments.");
                return Error.RuntimeError;
            }
            return try function.callable.call(data, self, arguments);
        }

        fn lookUpVariable(self: *Self, name: Token, expression: ast.Expr) !void {
            if (self.locals.get(expression)) |distance| {
                _ = distance;
                return self.environment.get(name);
            }
            self.global.get(name);
        }

        fn evaluate(self: *Self, expression: *const ast.Expr) Errors!Object {
            return switch (expression.*) {
                .literal => |literal| {
                    if (literal.value == null) return Object.nil;
                    switch (literal.value.?) {
                        .number => |num| return Object{ .number = num },
                        .string => |str| return Object{ .string = str },
                        .boolean => |boolean| return Object{ .boolean = boolean },
                    }
                },
                .grouping => |grouping| self.evaluate(grouping.expression),
                .unary => |unary| self.evalUnary(unary),
                .binary => |binary| self.evalBinary(binary),
                .variable => |variable| {
                    if (self.locals.get(expression)) |distance| {
                        return try self.environment.getAt(distance, variable.name);
                    } else return self.global.get(variable.name);
                },
                .assignment => |assignment| {
                    const value = try self.evaluate(assignment.value);
                    if (self.locals.get(expression)) |distance|
                        try self.environment.assignAt(distance, assignment.name, value)
                    else
                        try self.global.assign(assignment.name, value);
                    return value;
                },
                .logical => |logical| self.evalLogical(logical),
                .call => |call| self.evalCall(call),
                .get => |get| {
                    const object = try self.evaluate(get.object);
                    switch (@TypeOf(object)) {
                        Instance => {
                            var instance: *Instance = @ptrCast(@alignCast(object.callable.data));
                            instance.get(object.callable.data, get.name);
                        },
                        else => {
                            try _error(get.name, "Only instances have properties.");
                            return Error.RuntimeError;
                        },
                    }
                },
                .set => |set| {
                    const object = try self.evaluate(set.object);
                    switch (@TypeOf(object)) {
                        Instance => {
                            const value = try self.evaluate(set.value);
                            var instance: *Instance = @ptrCast(@alignCast(object.callable.data));
                            instance.set(object.callable.data, set.name, value);
                        },
                        else => {
                            try _error(set.name, "Only instances have fields.");
                            return Error.RuntimeError;
                        },
                    }
                },
            };
        }

        fn stringify(self: *Self, object: Object) ![]const u8 {
            return switch (object) {
                .nil => "nil",
                .string => |str| str,
                .number => |num| try std.fmt.allocPrint(
                    self.arena.allocator(),
                    "{d}",
                    .{num},
                ),
                .boolean => |boolean| switch (boolean) {
                    true => "true",
                    false => "false",
                },
                .callable => |callable| callable.toString(callable.data),
            };
        }

        fn expressionStatement(self: *Self, stmt: *const ast.Stmt) !void {
            _ = try self.evaluate(stmt.expression);
        }

        fn printStatement(self: *Self, stmt: *const ast.Stmt) !void {
            const value = try self.evaluate(stmt.print);
            try self.writer.print("{!s}\n", .{self.stringify(value)});
        }
        fn varStatement(self: *Self, stmt: ast.VarDecl) !void {
            var value: Object = .nil;
            if (stmt.intializer != null)
                value = try self.evaluate(stmt.intializer.?);
            try self.environment.define(stmt.name.lexeme, value);
        }
        fn blockStatement(self: *Self, block: ast.Block) Errors!?Object {
            const allocator = self.arena.allocator();
            const parent = self.environment;
            const child = try allocator.create(Env);
            child.* = Env.init(allocator, self.environment);
            self.environment = child;
            defer self.environment = parent;
            for (block.statements.items) |statement| {
                const return_value = try self.execute(&statement);
                if (return_value) |val| return val;
            }
            return null;
        }

        fn ifStatement(self: *Self, statement: ast.IfStmt) Errors!?Object {
            const truthy = self.truthVal(try self.evaluate(statement.condition));
            if (truthy) {
                const return_value = try self.execute(statement.thenBranch);
                if (return_value) |val| return val;
            } else if (statement.elseBranch != null) {
                const return_value = try self.execute(statement.elseBranch.?);
                if (return_value) |val| return val;
            }
            return null;
        }

        fn whileStatement(self: *Self, statement: ast.WhileStmt) Errors!?Object {
            while (self.truthVal(try self.evaluate(statement.condition))) {
                const return_value = try self.execute(statement.body);
                if (return_value) |val| return val;
            }
            return null;
        }

        fn funStatement(self: *Self, statement: ast.FunDecl) Errors!void {
            const function = try self.arena.allocator().create(Function);
            function.* = Function{
                .allocator = self.arena.allocator(),
                .declaration = statement,
                .closure = self.environment,
            };
            const callable = function.init();
            try self.environment.define(
                statement.name.lexeme,
                .{ .callable = callable },
            );
        }

        fn classStatement(self: *Self, statement: ast.ClassDecl) Errors!void {
            const allocator = self.arena.allocator();
            try self.environment.define(
                statement.name.lexeme,
                Object.nil,
            );
            const class = try allocator.create(Class);
            class.* = Class{
                .allocator = allocator,
                .name = statement.name.lexeme,
            };
            const callable = class.init();
            try self.environment.assign(
                statement.name,
                .{ .callable = callable },
            );
        }

        fn returnStatement(self: *Self, statement: ast.ReturnStmt) Errors!Object {
            return if (statement.value) |val|
                try self.evaluate(val)
            else
                .nil;
        }

        fn execute(self: *Self, statement: *const ast.Stmt) !?Object {
            try switch (statement.*) {
                .expression => self.expressionStatement(statement),
                .print => self.printStatement(statement),
                .variable => |_var| self.varStatement(_var),
                .block => |block| return try self.blockStatement(block),
                ._if => |_if| return try self.ifStatement(_if),
                ._while => |_while| return try self.whileStatement(_while),
                .function => |fun| self.funStatement(fun),
                .class => |class| self.classStatement(class),
                ._return => |_return| return try self.returnStatement(_return),
            };
            return null;
        }

        pub fn resolve(self: *Self, expression: *const ast.Expr, depth: u64) !void {
            try self.locals.put(expression, depth);
        }

        pub fn interpret(self: *Self, statements: []const *ast.Stmt) !void {
            for (statements) |statement| _ = try self.execute(statement);
        }
    };
}
