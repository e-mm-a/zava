from dataclasses import dataclass
from contextlib import contextmanager
from syntax import *

class Type:
    def __repr__(self):
        match self:
            case TArr(t):
                return f"[{t}]"
            case TCall(t, ts):
                return f"{t}(" + ", ".join(map(repr, ts)) + ")"
            case TPrim(name):
                return name
            case TFunc(ts, t):
                return "(" + ", ".join(map(repr, ts)) + f") -> {t}"
            case TClass(name, _):
                return name

@dataclass(repr=False)
class TArr(Type):
    contains: Type

@dataclass(repr=False)
class TCall(Type):
    name: str
    args: list[Type]

@dataclass(repr=False)
class TPrim(Type):
    name: str

@dataclass(repr=False)
class TFunc(Type):
    args: list[Type]
    ret: Type

@dataclass(repr=False)
class TClass(Type):
    name: str
    env: dict[str, Type]

class CheckError(Exception):
    ...

class Check:

    envs = []
    ret = None
    
    classes = [{}]
    cls = None

    errors = []

    def __init__(self, filename, src):
        self.filename = filename
        self.src = src.split("\n")

    @contextmanager
    def new_scope(self):
        self.envs.append({})
        yield
        self.envs.pop()

    @contextmanager
    def gather_errors(self):
        try:
            yield
        except CheckError as e:
            self.errors.append(e)

    def format_errors(self):
        out = []
        for err in self.errors:
            msg, loc = err.args
            gap = " "*(len(str(loc.line)) + 1)
            line = self.src[loc.line - 1]
            arrow = " "*(loc.column-1) + "^"*(loc.end_column-loc.column)

            out.append(
                f"{self.filename}:{loc.line}:{loc.column}:\n" +
                f"{gap}|\n" +
                f"{loc.line} | {line}\n" +
                f"{gap}| " + arrow + "\n" +
                msg
            )
        return "\n\n".join(out)

    def has_type(self, ast, type):
        t = self.check(ast)
        if t != type:
            raise CheckError(f"Expected type '{type}' but found '{t}'", ast.loc)
        return t

    def check_var_decl(self, decl, env):
        if decl.name in env:
            raise CheckError(f"Cannot redeclare existing variable '{decl.name}'", name.loc)
        if decl.sig:
            t = self.has_type(decl.value, self.check(decl.sig))
        else:
            t = self.check(decl.value)
        env[decl.name] = t

    def check_lit(self, lit):
        match lit:
            case LStr(_):
                return self.classes["String"]
            case LChar(_):
                return TPrim("char")
            case LNum(n):
                try:
                    int(n)
                    return TPrim("i32")
                except ValueError:
                    return TPrim("f32")
            case _:
                raise UserWarning(lit)

    def check_typesig(self, typesig):
        match typesig:
            case TsArr(t):
                return TArr(self.check(t))
            case TsCall(t, ts):
                return TCall(self.check(t), [self.check(t) for t in ts])
            case TsVar(name):
                prims = (
                    "i8", "i16", "i32", "i64",
                    "f32", "f64",
                    "char",
                    "void",
                )
                if name in prims:
                    return TPrim(name)

                for c_env in self.classes:
                    if name in c_env:
                        return c_env[name]
                raise CheckError(f"Unbound type '{name}'", typesig.loc)
            case _:
                raise UserWarning(typesig)

    def check_expr(self, expr):
        t = self._check_expr_impl(expr)
        expr.type = t
        return t

    def _check_expr_impl(self, expr):
        match expr:
            case EAssign(lhs, _, rhs):
                return self.has_type(rhs, self.check(lhs))
            case ECast(target, sig):
                self.check(target)
                return self.check(sig)
            case EOp(lhs, op, rhs):
                t = self.has_type(rhs, self.check(lhs))
                if op in ("==", "!=", "<", ">", "<=", ">="):
                    t = TPrim("bool")
                return t
            case ECall(fn, args):
                match self.check(fn):
                    case TFunc(params, ret):
                        args = []
                        for p, a in zip(params, args):
                            self.has_type(a, p)
                        return ret
                    case t:
                        raise CheckError(f"Cannot call non-function type '{t}'", fn.loc)
            case EDot(target, attr):
                match self.check(target):
                    case TClass(n, env) if attr in env:
                        return env[attr]
                    case t:
                        raise CheckError(f"Type '{t}' has no attribute '{attr}'", target.loc)
            case ELit(lit):
                return self.check(lit)
            case EVar(name):
                for e in self.envs + self.classes:
                    if name in e:
                        return e[name]
                raise CheckError(f"Unbound symbol '{name}'", expr.loc)
            case _:
                raise UserWarning(expr)

    def check_stmt(self, stmt):
        match stmt:
            case SDecl(var_decl):
                self.check_var_decl(var_decl, self.envs[-1])
            case SBreak() | SContinue():
                ...
            case SReturn(value):
                self.has_type(value, self.ret)
            case SExpr(expr):
                self.check(expr)

            case SBlock(stmts):
                with self.new_scope():
                    with self.gather_errors():
                        for s in stmts:
                            self.check(s)
            case SIf(cond, body, orelse):
                self.has_type(cond, TPrim("bool"))
                self.check(body)
                if orelse:
                    self.check(orelse)
            case SWhile(cond, step, body):
                self.has_type(cond, TPrim("bool"))
                if step:
                    self.check(step)
                self.check(body)
            case _:
                raise UserWarning(stmt)

    def check_decl(self, decl):
        match decl:
            case DDecl(var_decl):
                self.check_var_decl(var_decl, self.classes[-1])
            case DClass(mod, name, body):
                c_env = {}
                before = self.cls
                self.cls = TClass(name, c_env)
                self.classes.append(c_env)

                cs = list(filter(lambda d: isinstance(d, DClass), body))
                ds = list(filter(lambda d: isinstance(d, DDecl), body))
                fs = list(filter(lambda d: isinstance(d, DFunc), body))

                for c in cs:
                    self.check_decl(c)
                for d in ds:
                    self.check_var_decl(d)
                for f in fs:
                    ts = []
                    for t in [a[1] for a in f.args] + [f.ret]:
                        with self.gather_errors():
                            ts.append(self.check(t))
                    self.classes[-1][f.name] = TFunc(ts[:-1], ts[-1])
                for f in fs:
                    self.check_decl(f)

                self.classes.pop()
                self.classes[-1][name] = self.cls
                self.cls = before
            case DFunc(mod, name, args, ret, body):
                assert name in self.classes[-1]
                if body == ";":
                    return
                with self.new_scope():
                    for a, t in args:
                        with self.gather_errors(): 
                            self.envs[-1][a] = self.check(t)
                    with self.gather_errors():
                        self.ret = self.check(ret)
                    with self.gather_errors():
                        for s in body.stmts:
                            self.check(s)

    def check(self, ast):
        match ast:
            case Lit():
                return self.check_lit(ast)
            case TypeSig():
                return self.check_typesig(ast)
            case Expr():
                return self.check_expr(ast)
            case Stmt():
                 self.check_stmt(ast)
            case Decl():
                self.check_decl(ast)
            case _:
                raise UserWarning(ast)

    def check_file(self, file):
        for cls in file.decls:
            self.check(cls)

        if self.errors:
            print(self.format_errors())
            exit(1)
