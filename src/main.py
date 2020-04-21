#!/usr/bin/env python3
from pyswip import Prolog
from Compiler.lexer import lexer_job,set_up_env

def create_parse_tree(lex):

    prolog.consult("Compiler/parseTree.pl")

    query = "program(P,{},[])."

    parsetree = prolog.query(query.format(lex))

    return next(parsetree)["P"]

def give_semantics(parse_tree):
    prolog.consult("Runtime/runtime.pl")
    query = "program_eval({},Z)"

    Env = prolog.query(query.format(parse_tree))

    print(next(Env)["Z"])


# set_up_env()
prolog = Prolog()
lex = lexer_job()
parse_tree = create_parse_tree(lex)
give_semantics(parse_tree)
