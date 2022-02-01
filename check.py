from dataclasses import dataclass
from typing import cast, overload, Iterator, Union
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


@dataclass(repr=False, eq=False)
class TClass(Type):
    name: str
    env: dict[str, Type]

    def __eq__(self, other: object) -> bool:
        match other:
            case TClass(name, _):
                return self.name == name
        return False


prims = (
    TPrim("i8"),
    TPrim("i16"),
    TPrim("i32"),
    TPrim("i64"),
    TPrim("f32"),
    TPrim("f64"),
    TPrim("char"),
    TPrim("void"),
    TPrim("bool"),
)

wrappers = (
    TClass("Byte", {}),
    TClass("Short", {}),
    TClass("Integer", {}),
    TClass("Long", {}),
    TClass("Float", {}),
    TClass("Double", {}),
    TClass("Character", {}),
    TClass("Void", {}),
    TClass("Bool", {}),
)

t_bool = TPrim("bool")
t_void = TPrim("void")


class CheckError(Exception):
    ...


Env = dict[str, Type]


class Check:

    filename: str
    src: list[str]

    envs: list[Env]
    ret: Type

    classes: list[tuple[Type, Env]]

    errors: list[CheckError]

    def __init__(self) -> None:
        self.envs = []
        self.classes = [(cast(Type, None), {})]
        self.errors = []

    @contextmanager
    def new_scope(self) -> Iterator[None]:
        self.envs.append({})
        yield
        self.envs.pop()

    @contextmanager
    def gather_errors(self) -> Iterator[None]:
        try:
            yield
        except CheckError as e:
            self.errors.append(e)

    def format_errors(self) -> str:
        out = []
        for err in self.errors:
            msg, loc = err.args
            gap = " " * (len(str(loc.line)) + 1)
            line = self.src[loc.line - 1]
            arrow = " " * (loc.column - 1) + "^" * (loc.end_column - loc.column)

            out.append(
                f"{self.filename}:{loc.line}:{loc.column}:\n"
                + f"{gap}|\n"
                + f"{loc.line} | {line}\n"
                + f"{gap}| "
                + arrow
                + "\n"
                + msg
            )
        return "\n\n".join(out)

    def upcast(self, t1: Type, t2: Type) -> Type | None:
        def idx(t: Type, ts: list[tuple[Type, ...]]) -> int | None:
            for i, group in enumerate(ts):
                if t in group:
                    return i
            return None

        if t1 == t2:
            return t1

        cast: list[tuple[Type, ...]] = list(zip(prims, wrappers))

        for c in (cast[:4], cast[4:6]):
            i = idx(t1, c)
            j = idx(t2, c)
            if i is not None and j is not None:
                if i >= j:
                    return t1
                else:
                    return t2

        return None

    def has_type(self, ast: Lit | Expr | TypeSig, type: Type) -> Type:
        t = self.check(ast)
        out = self.upcast(t, type)
        if out:
            return out
        else:
            raise CheckError(f"Expected type '{type}' but found '{t}'", ast.loc)

    def check_var_decl(self, decl: VarDecl, env: Env) -> None:
        if decl.name in env:
            raise CheckError(
                f"Cannot redeclare existing variable '{decl.name}'", decl.loc
            )
        if decl.sig and decl.value:
            t = self.has_type(decl.value, self.check(decl.sig))
        elif decl.sig:
            t = self.check(decl.sig)
        elif decl.value:
            t = self.check(decl.value)
        env[decl.name] = t

    def check_lit(self, lit: Lit) -> Type:
        match lit:
            case LStr(_):
                return self.classes[0][1]["String"]
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

    def check_typesig(self, typesig: TypeSig) -> Type:
        t = self._check_typesig_impl(typesig)
        typesig.annot = t
        return t

    def _check_typesig_impl(self, typesig: TypeSig) -> Type:
        match typesig:
            case TsArr(t):
                return TArr(self.check(t))
            case TsCall(t, ts):
                return TCall(t, [self.check(t) for t in ts])
            case TsVar(name):
                p = TPrim(name)
                if p in prims:
                    return p

                for _, c_env in self.classes:
                    if name in c_env:
                        return c_env[name]
                raise CheckError(f"Unbound type '{name}'", typesig.loc)
            case _:
                raise UserWarning(typesig)

    def check_expr(self, expr: Expr) -> Type:
        t = self._check_expr_impl(expr)
        expr.annot = t
        return t

    def _check_expr_impl(self, expr: Expr) -> Type:
        match expr:
            case EAssign(lhs, _, rhs):
                return self.has_type(rhs, self.check(lhs))
            case ECast(target, sig):
                self.check(target)
                return self.check(sig)
            case EOp(lhs, op, rhs):
                tl = self.check(lhs)
                tr = self.check(rhs)
                arith = tuple(prims[:6]) + tuple(wrappers[:6])
                other = arith + prims[-3:] + (wrappers[-3], wrappers[-1])
                string = TClass("String", {})
                valid_str = (
                    op == "+"
                    and (tl == string or tr == string)
                    and (tl in arith or tr in arith)
                )
                valid_arith = (
                    op in ("+", "-", "*", "/", "%", "<", ">", "<=", ">=")
                    and tl in arith
                    and tr in arith
                )
                valid_other = op in ("==", "!=") and tl in other and tr in other
                if valid_str or valid_arith or valid_other:
                    if op in ("==", "!=", "<", ">", "<=", ">="):
                        return t_bool
                    else:
                        return cast(Type, self.upcast(tl, tr))
                else:
                    raise CheckError(
                        f"Unexpected types '{tl}' and '{tr}' to binary operand {op}",
                        expr.loc,
                    )
            case ECall(fn, args):
                match self.check(fn):
                    case TFunc(params, ret):
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
                        raise CheckError(
                            f"Type '{t}' has no attribute '{attr}'", target.loc
                        )
                # mypy bug: it thinks there needs to be a return here
                return cast(Type, None)
            case ELit(lit):
                return self.check(lit)
            case EVar(name):
                for e in self.envs + [c[1] for c in self.classes]:
                    if name in e:
                        return e[name]
                raise CheckError(f"Unbound symbol '{name}'", expr.loc)
            case _:
                raise UserWarning(expr)

    def check_stmt(self, stmt: Stmt) -> None:
        match stmt:
            case SDecl(var_decl):
                self.check_var_decl(var_decl, self.envs[-1])
            case SBreak() | SContinue():
                ...
            case SReturn(value):
                if value:
                    self.has_type(value, self.ret)
                elif self.ret != t_void:
                    raise CheckError("Expected return value", stmt.loc)
            case SExpr(expr):
                self.check(expr)

            case SBlock(stmts):
                with self.new_scope():
                    with self.gather_errors():
                        for s in stmts:
                            self.check(s)
            case SIf(cond, body, orelse):
                self.has_type(cond, t_bool)
                self.check(body)
                if orelse:
                    self.check(orelse)
            case SWhile(cond, step, body):
                self.has_type(cond, t_bool)
                if step:
                    self.check(step)
                self.check(body)
            case _:
                raise UserWarning(stmt)

    def check_decl(self, decl: Decl) -> None:
        match decl:
            case DDecl(var_decl):
                self.check_var_decl(var_decl, self.classes[-1][1])
            case DClass(mod, name, body):
                c_env: Env = {}
                cls = TClass(name, c_env)
                self.classes.append((cls, c_env))

                cs = list(filter(lambda d: isinstance(d, DClass), body))
                ds = list(filter(lambda d: isinstance(d, DDecl), body))
                fs = list(filter(lambda d: isinstance(d, DFunc), body))

                for d in cs + ds:
                    self.check_decl(d)
                for f in fs:
                    f = cast(DFunc, f)
                    ts = []
                    for t in [a[1] for a in f.args] + [f.ret]:
                        with self.gather_errors():
                            ts.append(self.check(t))
                    self.classes[-1][1][f.name] = TFunc(ts[:-1], ts[-1])
                for f in fs:
                    self.check_decl(f)

                self.classes.pop()
                self.classes[-1][1][name] = cls
            case DFunc(mod, name, args, ret, body):
                assert name in self.classes[-1][1]
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

    @overload
    def check(self, _: Lit | TypeSig | Expr) -> Type:
        ...

    @overload
    def check(self, _: Stmt | Decl) -> None:
        ...

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

        return None

    def check_file(self, file: File, path: str, src: str) -> None:
        self.filename = path
        self.src = src.split("\n")

        for cls in file.decls:
            self.check(cls)

        if self.errors:
            print(self.format_errors())
            exit(1)
