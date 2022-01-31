from syntax import *

@data
class Type:
    TArr: ["Type"]
    TCall: [str, ["Type"]]
    TPrim: [str]
    TVar: [str]
    TFunc: [str, ["Type"], "Type"]
    TCls: [str]

class Check:
    def __init__(self, src):
        self.src = src

    def check_lit(self, lit):
        match lit:
            case LStr(_):
                return TVar("String")
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
            case TSArr(t):
                return TArr(self.check_typesig(t))
            case TSCall(t, ts):
                return TCall(self.check_typesig(t), [self.check_typesig(t) for t in ts])
            case TSPrim(n):
                return TPrim(n)
            case TSVar(n):
                raise NotImplementedError
            case _:
                raise UserWarning(typesig)

    def check_expr(self, expr):
        t = self._check_expr_impl(expr)
        expr.type = t
        return t

    def _check_expr_impl(self, expr):
        match expr:
            case EAssign(lhs, op, rhs):
                ...
            case ECast(target, type):
                ...
            case EOp(lhs, op, rhs):
                ...
            case ECall(fn, args):
                ...
            case EDot(target, attr):
                ...
            case ELit(lit):
                ...
            case EVar(name):
                ...
            case _:
                raise UserWarning(expr)

    def check_stmt(self, stmt):
        match stmt:
            case SDecl(name, type, value):
                ...
            case SBreak() | SContinue():
                ...
            case SReturn(value):
                ...
            case SExpr(expr):
                ...

            case SBlock(stmts):
                ...
            case SIf(cond, body, orelse):
                ...
            case SWhile(cond, step, body):
                ...
            case _:
                raise UserWarning(stmt)

    def check_decl(self, decl):
        match decl:
            case DDecl(name, type, value):
                ...
            case DFunc(mod, name, args, ret, body):
                ...

    def check(self, ast):
        match ast:
            case Lit(lit):
                return self.check_lit(lit)
            case TypeSig(sig):
                return self.check_typesig(sig)
            case Expr(expr):
                return self.check_expr(expr)
            case Stmt(stmt):
                return self.check_stmt(stmt)
            case Decl(decl):
                return self.check_decl(decl)
            case _:
                raise UserWarning(ast)

    def check_class(self, cls):
        ...

    def check_file(self, file):
        for cls in file.classes:
            self.check_class(cls)
