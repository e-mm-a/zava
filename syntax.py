from dataclasses import dataclass, make_dataclass

def data(cls):
    for k, v in cls.__annotations__.items():
        d = make_dataclass(
            k,
            [(f"u{i}", v) for i, v in enumerate(v)],
            bases=(cls,),
            repr=False,
        )
        globals()[k] = d
        setattr(cls, k, d)
    return cls

def Many(*x):
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
    DDecl: [str, TypeSig | None, Expr | None]
    DFunc: [[str], str, [(str, TypeSig)], TypeSig, Stmt]

@dataclass
class Class:
    modifiers: [str]
    name: str
    decls: [Decl]

@dataclass
class File:
    classes: [Class]
