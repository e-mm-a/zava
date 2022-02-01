from dataclasses import dataclass
from contextlib import contextmanager
from syntax import *

@data
class Type:
    Arr: ["Type"]
    Call: [str, ["Type"]]
    Prim: [str]
    Func: [["Type"], "Type"]

    def __repr__(self):
        match self:
            case Type.Arr(t):
                return f"[{t}]"
            case Type.Call(t, ts):
                return f"{t}(" + ", ".join(map(repr, ts)) + ")"
            case Type.Prim(name):
                return name
            case Type.Func(ts, t):
                return "(" + ", ".join(map(repr, ts)) + f") -> {t}"
            case TCls(name, _):
                return name

@dataclass
class TCls(Type):
    name: str
    env: dict[str, Type]

class CheckError(Exception):
    ...

class Check:

    envs = []
    ret = None
    
    classes = {}
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

    def check_var_decl(self, name, type, value, env):
        if name in env:
            raise CheckError(f"Cannot redeclare existing variable '{name}'", name.loc)
        if type:
            t = self.has_type(value, self.check(type))
        else:
            t = self.check(value)
        env[name] = t

    def check_lit(self, lit):
        match lit:
            case LStr(_):
                return self.classes["String"]
            case LChar(_):
                return Type.Prim("char")
            case LNum(n):
                try:
                    int(n)
                    return Type.Prim("i32")
                except ValueError:
                    return Type.Prim("f32")
            case _:
                raise UserWarning(lit)

    def check_typesig(self, typesig):
        match typesig:
            case TSArr(t):
                return Type.Arr(self.check(t))
            case TSCall(t, ts):
                return Type.Call(self.check(t), [self.check(t) for t in ts])
            case TSVar(name):
                prims = (
                    "i8", "i16", "i32", "i64",
                    "f32", "f64",
                    "char",
                    "void",
                )
                if name in prims:
                    return Type.Prim(name)
                elif name in self.classes:
                    return self.classes[name]
                else:
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
            case ECast(target, type):
                self.check(target)
                return self.check(type)
            case EOp(lhs, op, rhs):
                t = self.has_type(rhs, self.check(lhs))
                if op in ("==", "!=", "<", ">", "<=", ">="):
                    t = Type.Prim("bool")
                return t
            case ECall(fn, args):
                match self.check(fn):
                    case Type.Func(params, ret):
                        args = []
                        for p, a in zip(params, args):
                            self.has_type(a, p)
                        return self.check(ret)
                    case t:
                        raise CheckError(f"Cannot call non-function type '{t}'", fn.loc)
            case EDot(target, attr):
                match self.check(target):
                    case TCls(env) if attr in env:
                        return env[attr]
                    case t:
                        raise CheckError(f"Type '{t}' has no attribute '{attr}'", target.loc)
            case ELit(lit):
                return self.check(lit)
            case EVar(name):
                for e in self.envs:
                    if name in e:
                        return e[name]
                raise CheckError(f"Unbound symbol '{name}'", expr.loc)
            case _:
                raise UserWarning(expr)

    def check_stmt(self, stmt):
        match stmt:
            case SDecl(name, type, value):
                self.check_var_decl(name, type, value, self.envs[-1])
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
                self.has_type(cond, Type.Prim("bool"))
                self.check(body)
                if orelse:
                    self.check(orelse)
            case SWhile(cond, step, body):
                self.has_type(cond, Type.Prim("bool"))
                if step:
                    self.check(step)
                self.check(body)
            case _:
                raise UserWarning(stmt)

    def check_decl(self, decl):
        match decl:
            case DDecl(name, type, value):
                self.check_var_decl(name, type, value, self.cls.env)
            case DClass(mod, name, body):
                self.cls = TCls(name, {})
                b = list(body)
                for i, d in enumerate(body):
                    match d:
                        case DClass():
                            with self.gather_errors():
                                self.check_decl(d)
                            b.pop(i)
                        case DFunc(_, name, args, ret, _):
                            ts = []
                            for t in [a[1] for a in args] + [ret]:
                                try:
                                    ts.append(self.check(t))
                                except CheckError as e:
                                    self.errors.append(e)
                                    b.pop(i)
                            self.cls.env[name] = Type.Func(ts[:-1], ts[-1])
                for d in b:
                    with self.gather_errors():
                        self.check(d)
                self.classes[name] = self.cls
            case DFunc(mod, name, args, ret, body):
                assert name in self.cls.env
                if body == ";":
                    return
                with self.new_scope():
                    for a, t in args:
                        with self.gather_errors(): 
                            self.envs[-1][a] = self.check(t)
                    with self.gather_errors():
                        self.ret = self.check(ret)
                    with self.gather_errors():
                        for s in body.u0:
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
