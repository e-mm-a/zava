import lark
from syntax import *

with open("zava.lark", "r") as f:
    parser = lark.Lark(f, start="file", propagate_positions=True)

def transform(tree):
    match type(tree):
        case lark.Tree:
            if tree.data.startswith("op_"):
                n = "EOp"
            elif tree.data[1] == "_":
                n = tree.data[0].upper() + tree.data[2:].capitalize()
            else:
                n = tree.data
            x = eval(n)(*map(transform, tree.children))
            try: x.loc = tree.meta
            except AttributeError: ...
            return x
        case lark.Token:
            return str(tree)
        case _:
            return tree

def parse(src):
    return transform(parser.parse(src))
