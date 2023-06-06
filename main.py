from pyparsing import *
ParserElement.enableLeftRecursion()

class Ast:
    def __init__(self, src, pos, tokens):
        self.src = src
        self.pos = pos
        self.lineno = 0
        self.column = 0
        # total = 0
        # for i, line in enumerate(src.split("\n")):
        #     if total + len(line) + 1 >= pos:
        #         self.lineno = i + 1
        #         self.column = pos - total
        #     total += len(line) + 1
        for ch in src[:pos]:
            if ch == "\n":
                self.lineno += 1
                self.column = 0
            else:
                self.column += 1

    def get_context(self):
        s = "\n".join(self.src.split("\n")[self.lineno-3:self.lineno]) + "\n"
        s += " " * (self.column) + "^ " + str(self.lineno) + ":" + str(self.column)
        return "\n" + s + "\n"

class Env:
    def __init__(self, outer=None):
        self.vars = {}
        self.outer = outer

    def get(self, key):
        if key in self.vars:
            return self.vars[key]
        if self.outer is not None:
            return self.outer.get(key)
        return None

    def set(self, key, val):
        self.vars[key] = val

    def __str__(self):
        return str(self.vars)

class Law(Ast):
    def __init__(self, src, pos, tokens):
        super().__init__(src, pos, tokens)
        self.name = tokens[0]
        self.event = tokens[1]
        self.event_args = tokens[2]
        self.statements = tokens[3:]

    def __str__(self):
        return f"law \"{self.name}\" {self.event}({', '.join(map(str, self.event_args))}) {{\n{chr(10).join(map(str, self.statements))}\n}}"

    def evaluate(self, params, globals):
        env = Env()
        env.vars = globals
        for arg, val in zip(self.event_args, params):
            env.set(arg.val, val)

        for smt in self.statements:
            smt.evaluate(env)

        return env

class Assignment(Ast):
    def __init__(self, src, pos, tokens):
        super().__init__(src, pos, tokens)
        self.lhs = tokens[0]
        self.rhs = tokens[1]

    def evaluate(self, env):
        env.set(self.lhs.val, self.rhs.evaluate(env))

    def __str__(self):
        return f" " * self.column + f"{self.lhs} = {self.rhs}"

class Ident(Ast):
    def __init__(self, src, pos, tokens):
        super().__init__(src, pos, tokens)
        self.val = tokens[0]

    def evaluate(self, env):
        val = env.get(self.val)
        if val is None:
            raise RuntimeError(f"Name '{self.val}' is not defined")
        return val

    def __str__(self):
        return self.val

class BinOp(Ast):
    def __init__(self, src, pos, tokens):
        super().__init__(src, pos, tokens)
        self.op = tokens[0][1]
        self.operands = tokens[0][::2]

    def __str__(self):
        return f" {self.op} ".join(map(str, self.operands))

    def evaluate(self, env):
        operands = [operand.evaluate(env) for operand in self.operands]
        result = operands[0]
        for operand in operands[1:]:
            if self.op == "+": result += operand
            elif self.op == "-": result -= operand
            elif self.op == "*": result *= operand
            elif self.op == "/": result /= operand
            elif self.op == "%": result %= operand
            elif self.op in ["is", "=="]: result = result == operand
            elif self.op in ["isn't", "!="]: result = result != operand
            else:
                raise RuntimeError(f"Unsupported operator '{self.op}'")

        return result

class Number(Ast):
    def __init__(self, src, pos, tokens):
        super().__init__(src, pos, tokens)
        self.val = float(tokens[0])

    def __str__(self): return str(self.val)
    def evaluate(self, env): return self.val

expr = Forward()
ident = Word(alphas + "_", alphanums + "_" + "'").set_parse_action(Ident)
number = Combine(Optional("-") + Word("0123456789") + Optional(Literal(".") + Word("0123456789"))).set_parse_action(Number)
primary_expr = number | ident
assign = (ident + Suppress("=") + expr).set_parse_action(Assignment)

mathexpr = infix_notation(primary_expr,
    [
        (oneOf("* / %"), 2, opAssoc.LEFT, BinOp),
        (oneOf("+ -"), 2, opAssoc.LEFT, BinOp),
        (oneOf(">= <= == > < != is isn't"), 2, opAssoc.LEFT, BinOp),
        (oneOf("= += -= *= /="), 2, opAssoc.LEFT, BinOp),
    ]
)


expr <<= mathexpr | Suppress('(') + expr + Suppress(')') | primary_expr

clause = assign
law = (Suppress("law") +
       Suppress('"') + Regex(r'[^"]*') + Suppress('"') +
       ident + Group(Optional(Suppress("(") + delimited_list(ident) + Suppress(")"))) +
       Suppress("{") + ZeroOrMore(clause) + Suppress("}")).set_parse_action(Law)

ast = law.parse_string('''law "communists are great" when_citizen_paid(a, b, c) {
    income = b is c
    tax = b isn't c

}''', parse_all=True)[0]

print(ast)

citizens = ["ImmAdam", "KingJellyfish", "LukePiwalker31", "Matchstick8"]
def bench_func():
    return ast.evaluate([1, 2, 3], {"citizens": citizens})

import timeit
# print(timeit.timeit("bench_func()", globals=locals())/1000000)

print(bench_func())