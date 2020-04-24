#!/usr/bin/env python3
from nltk.tokenize import sent_tokenize, word_tokenize
from functools import reduce
from tokenize import tokenize, untokenize, NUMBER, STRING, NAME, OP
from io import BytesIO

def lexer(file):
    '''
    This method will take generate tokens and 

    Arguments : 
        file : 
            type : str
            desc : Input file name
    Return : 
        type : str
        desc : token generated list format
    '''
    input = open(file, 'r').read()
    result = []
    lex = "["
    tokenize_str = tokenize(BytesIO(input.encode('utf-8')).readline) 
    prev=""
    for toknum, tokval, _, _, _ in tokenize_str:
        if(len(tokval)!=0):
            ascii = reduce(lambda x, y: str(x)+str(y), map(ord, tokval))
            if ascii != 10 and tokval != "utf-8" and ascii != "32323232" and ascii != 9:
                # print(str(type(tokval))+" "+str(ascii)+" tokval = "+tokval)
                if tokval==')' or tokval=='(' or tokval=='{' or tokval=='}' or tokval=='!=':
                    lex += "'"+tokval+"'"
                else:
                    lex+=tokval
                lex+=","
                prev=tokval
    lex = lex[:-1]
    lex += ']'
    print(lex)
    return lex
