#!/usr/bin/env python3
from pyswip import Prolog
from Compiler.lexer import lexer_job,set_up_env,lexer
import sys
def create_parse_tree(lex):

    prolog.consult("Compiler/parseTree.pl")

    query = "program(P,{},[])."

    parsetree = prolog.query(query.format(lex))

    x = next(parsetree)["P"]
    y = x.replace("(b'","('")

    return y

def give_semantics(parse_tree):
    prolog.consult("Runtime/runtime.pl")
    query = "program_eval({},Z)"

    Env = prolog.query(query.format(parse_tree))
    # print(list(Env))
    print(next(Env)["Z"])

def get_arg():
	file = ""
	try:
		file = sys.argv[1]
	except:
		print("Error \nScript Usage -> python3 main.py inputfile")
		sys.exit(2)
	return file

set_up_env()
prolog = Prolog()
file = get_arg()
lex = lexer(file)
parse_tree = create_parse_tree(lex)
give_semantics(parse_tree)
