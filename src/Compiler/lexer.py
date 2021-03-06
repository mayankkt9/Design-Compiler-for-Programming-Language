#!/usr/bin/env python3
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

    input = open(file, 'r')
    input_comment_removed = open(file+"_rem", 'w')
    result = []
    lex = "["
    for line in input:
        if not line.startswith('#'):
            input_comment_removed.write(line)
    input_comment_removed.close()
    process = open(file+"_rem", 'r').read()
    tokenize_str = tokenize(BytesIO(process.encode('utf-8')).readline)
    identify_list = []
    for toknum, tokval, _, _, _ in tokenize_str:
        if(len(tokval) != 0):
            ascii = reduce(lambda x, y: str(x)+str(y), map(ord, tokval))
            if ascii != 10 and tokval != "utf-8" and ascii != "32323232" and ascii != 9:
                if tokval == '[':
                    identify_list.append(tokval)
                    continue
                elif tokval == ']':
                    identify_list.append(tokval)
                    lex += ("".join(identify_list))
                    identify_list = []
                elif identify_list:
                    identify_list.append(tokval)
                    continue
                else:
                    if tokval == '!=':
                        lex += '"'+tokval+'"'
                    elif tokval == ')' or tokval == '(' or tokval == '{' or tokval == '}':
                        lex += "'"+tokval+"'"
                    else:
                        lex += tokval
                lex += ","
    lex = lex[:-1]
    lex += ']'
    # print(lex)
    return lex
