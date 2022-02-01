from typing import cast
from syntax import *
from check import *

prim_names = {
    "i8": "byte",
    "i16": "short",
    "i32": "int",
    "i64": "long",
    "f32": "float",
    "f64": "double",
    "rune": "char",
    "bool": "bool",
    "void": "void",
}

mod_names = {
    "pub": "public",
    "static": "static",
}


def mods(ms: list[str]) -> str:
    m = " ".join([mod_names[m] for m in ms])
    if m:
        m += " "
    return m


def parens(x: str) -> str:
    return "(" + x + ")"


def indent(x: str) -> str:
    return "\n".join(f"    {line}" for line in x.split("\n"))


def block(xs):
    return "{\n" + "\n".join(map(indent, xs)) + "\n}"


class CodeGen:
    def gen_type(self, type: Type) -> str:
        match type:
            case TArr(contains):
                return self.gen_type(contains) + "[]"
            case TCall(target, args):
                return f"{target}<" + ", ".join(map(self.gen_type, args)) + ">"
            case TPrim(name):
                return prim_names[name]
            case TFunc(ts, t):
                raise UserWarning("Tried to generate function type code")
            case TClass(name, _):
                return name
            case _:
                raise UserWarning(type)

    def gen_var_decl(self, decl: VarDecl) -> str:
        suffix = ""
        if decl.sig:
            t = decl.sig.annot
        else:
            t = cast(Expr, decl.value).annot
        while isinstance(t, TArr):
            t = t.contains
            suffix += "[]"
        if decl.value:
            suffix += " = " + self.gen_expr(decl.value)
        return self.gen_type(t) + f" {decl.name}{suffix}"

    def gen_lit(self, lit: Lit) -> str:
        return str(lit.value)

    def gen_typesig(self, typesig: TypeSig) -> str:
        return self.gen_type(typesig.annot)

    def gen_expr(self, expr: Expr) -> str:
        match expr:
            case EAssign(lhs, op, rhs) | EOp(lhs, op, rhs):
                return " ".join([self.gen_expr(lhs), str(op), self.gen_expr(rhs)])
            case ECast(target, sig):
                return parens(self.gen_typesig(sig)) + self.gen_expr(target)
            case ECall(fn, args):
                return self.gen_expr(fn) + parens(", ".join(map(self.gen_expr, args)))
            case EDot(target, attr):
                return self.gen_expr(target) + f".{attr}"
            case ELit(lit):
                return self.gen_lit(lit)
            case EVar(name):
                return name
            case _:
                raise UserWarning(expr)

    def gen_stmt(self, stmt: Stmt) -> str:
        match stmt:
            case SDecl(var_decl):
                return self.gen_var_decl(var_decl) + ";"
            case SBreak():
                return "break;"
            case SContinue():
                return "continue;"
            case SReturn(value):
                v = (" " + self.gen_expr(value)) if value else ""
                return f"return{v};"
            case SExpr(expr):
                return self.gen_expr(expr) + ";"

            case SBlock(stmts):
                return block(map(self.gen_stmt, stmts))
            case SIf(cond, body, orelse):
                i = f"if " + parens(self.gen_expr(cond)) + " " + self.gen_stmt(body)
                if orelse:
                    i += " else " + self.gen_stmt(orelse)
                return i
            case SWhile(cond, step, body):
                b: Stmt
                if step:
                    s = SExpr(step)
                    match body:
                        case SBlock(stmts):
                            b = SBlock(list(stmts) + [s])
                        case _:
                            b = SBlock([body, s])
                else:
                    b = body
                return f"while " + parens(self.gen_expr(cond)) + " " + self.gen_stmt(b)
            case _:
                raise UserWarning(stmt)

    def gen_decl(self, decl: Decl) -> str:
        match decl:
            case DDecl(var_decl):
                return self.gen_var_decl(var_decl)
            case DClass(mod, name, body):
                return (
                    mods(mod) + " class " + f"{name} " + block(map(self.gen_decl, body))
                )
            case DFunc(mod, name, args, ret, body):
                if body == ";":
                    return ";"
                a = parens(
                    ", ".join(self.gen_var_decl(VarDecl(n, t, None)) for n, t in args)
                )
                b = self.gen_stmt(body)
                return mods(mod) + self.gen_typesig(ret) + f" {name}{a} " + b
            case _:
                raise UserWarning(decl)

    def gen_file(self, file: File) -> str:
        return "\n\n".join(map(self.gen_decl, file.decls))
