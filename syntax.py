from dataclasses import dataclass, make_dataclass

def data(cls):
    for k, v in cls.__annotations__.items():
        d = make_dataclass(k, [(f"u{i}", v) for i, v in enumerate(v)], bases=(cls,))
        globals()[k] = d
        setattr(cls, k, d)
    return cls

def many(*x):
    return x

@data
class Lit:
    LStr: [str]
    LChar: [str]
    LNum: [int | float]

@data
class TypeSig:
    TSArr: ["TypeSig"]
    TSCall: [str, ["TypeSig"]]
    TSPrim: [str]
    TSVar: [str]

@data
class Expr:
    EAssign: ["Expr", str, "Expr"]
    ECast: ["Expr", TypeSig]
    EOp: ["Expr", str, "Expr"]
    ECall: ["Expr", ["Expr"]]
    EDot: ["Expr", str]
    ELit: [Lit]
    EVar: [str]

@data
class Stmt:
    SDecl: [str, TypeSig | None, Expr | None]
    SBreak: []
    SContinue: []
    SReturn: [Expr | None]
    SExpr: [Expr]

    SBlock: [["Stmt"]]
    SIf: [Expr, "Stmt", "Stmt"]
    SWhile: [Expr, Expr | None, "Stmt"]

@data
class Decl:
    DClass: [[str], str, [Stmt], [0]]
    DFunc: [[str], str, [(str, TypeSig)], TypeSig, Stmt]

@dataclass
class file:
    decls: [Decl]
