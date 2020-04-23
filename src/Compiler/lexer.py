#!/usr/bin/env python3
from nltk.tokenize import sent_tokenize, word_tokenize
from functools import reduce
import nltk
from tokenize import tokenize, untokenize, NUMBER, STRING, NAME, OP
from io import BytesIO

def lexer(file):
    str2 = open(file, 'r').read()
    result = []
    c123 = ""
    lex = "["
    str1 = tokenize(BytesIO(str2.encode('utf-8')).readline) 
    prev=""
    for toknum, tokval, _, _, _ in str1:
        if toknum == NUMBER and '.' in tokval:  # replace NUMBER tokens
            result.extend([
                (NAME, 'Decimal'),
                (OP, '('),
                (STRING, repr(tokval)),
                (OP, ')')
            ])
        else:
            if(len(tokval)!=0):
                ascii = reduce(lambda x, y: str(x)+str(y), map(ord, tokval))
                if ascii != 10 and tokval != "utf-8" and ascii != "32323232" and ascii != 9:
                    # print(str(type(tokval))+" "+str(ascii)+" tokval = "+tokval)
                    if prev=='(' and tokval[0]=='"':
                        lex+="'\"',"
                    if tokval==')' and prev[len(prev)-1]=='"':
                        lex+="'\"',"
                    if tokval==')' or tokval=='(' or tokval=='{' or tokval=='}':
                        lex += "'"+tokval+"'"
                    else:
                        lex+=tokval
                    lex+=","
                    
                    prev=tokval
                # result.append((toknum, tokval))
    lex = lex[:-1]
    lex += ']'
    print(lex)
    return lex

def lexer_job(file):
    str2 = open(file, 'r').read()
    str1 = word_tokenize(str2)
    lex = ""
    lex += '['
    for i in range(0, len(str1)):
        ascii = reduce(lambda x, y: str(x)+str(y), map(ord, str1[i]))
        ascii_prev = 0
        if i > 1:
            ascii_prev = reduce(lambda x, y: str(x)+str(y), map(ord, str1[i-1]))
        # print(str1[i]+ " "+str(ascii))
        if i > 1 and str1[i] == '=' and str1[i-1] == ':':
            continue
        if i > 3 and (ascii_prev == "9696" or ascii_prev == "3939") and str1[i-2]=='(' and str1[i-3]=="print":
            lex += '"'+str1[i]+'"'
        elif str1[i] == ':' and str1[i+1] == '=':
            lex += str1[i]+str1[i+1]
            i += 2
        elif str1[i] == '(':
            lex += "'('"
        elif str1[i] == ')':
            lex += "')'"
        elif ascii == "9696" or ascii == "3939":
            lex += "'\"'"
        elif str1[i] == '}':
            lex += "'}'"
        elif str1[i] == '{':
            lex += "'{'"
        else:
            lex += str1[i]
        lex += ','
    lex = lex[:-1]
    lex += ']'
    print("LEXER")
    print(lex)
    return lex


def set_up_env():
    try:
        nltk.data.find('tokenizers/punkt')
    except LookupError:
        nltk.download('punkt')
