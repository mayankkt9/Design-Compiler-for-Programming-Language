#!/usr/bin/env python3
from pyswip import Prolog
from Compiler.lexer import lexer
import sys
import os


def create_parse_tree(lex):
    '''
    This method will take tokens from the lexer and generate parse tree

    Arguments : 
        lex : 
            type : str
            desc : list of tokens
    Return : 
        type : str
        desc : parse tree generated after parsin the tokens
    '''

    prolog.consult("Compiler/parseTree.pl")
    query = "program(P,{},[])."
    parsetree = prolog.query(query.format(lex))
    x = next(parsetree)["P"]
    y = x.replace("b'", "'")
    return y


def give_semantics(parse_tree):
    '''
    This method will take parse tree as input and execute code

    Arguments : 
        parse_tree : 
            type : str
            desc : The input parse tree generated after parsing the code.
    Return Type : None
    '''

    prolog.consult("Runtime/runtime.pl")
    query = "program_eval({},Z)"
    Env = prolog.query(query.format(parse_tree))
    environment = next(Env)["Z"]
    # print(environment)


def get_arg():
    '''
    This method will get system arguments to run the code.
    The system argument here would be the input code in .rch format

    Arguments : None
    Return Type : file
    '''

    file = ""
    try:
        file = sys.argv[1]
    except:
        print("Error \nScript Usage -> python3 main.py inputfile")
        sys.exit(2)
    return file


if __name__ == "__main__":

    # Comment these lines if want to do bulk testing
    prolog = Prolog()
    file = get_arg()
    lex = lexer(file)
    file_remove = file + "_rem"
    try:
        parse_tree = create_parse_tree(lex)
    except StopIteration:
        print("Syntax Error")
        os.remove(file_remove) if os.path.exists(file_remove) else None
        sys.exit(0)
    try:
        give_semantics(parse_tree)
    except StopIteration:
        print("Semantics Error")
        os.remove(file_remove) if os.path.exists(file_remove) else None
        sys.exit(0)
    os.remove(file_remove) if os.path.exists(file_remove) else None
