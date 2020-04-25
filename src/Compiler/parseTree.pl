% Parse Tree Generator
%:- use_rendering(svgtree).

:- use_module(library(tabling)).
:- table expr_op/3, term/3, bool/3.

% Expressions
expr(t_assign(X, Y)) --> identifier(X), [=], expr_op(Y).
expr(X) --> expr_op(X).

expr_op(t_add(X, Y))-->expr_op(X), [+], term(Y).
expr_op(t_sub(X, Y))-->expr_op(X), [-], term(Y).
expr_op(X) --> term(X).

term(t_div(X, Y))-->term(X), [/], brackets(Y).
term(t_mul(X, Y)) --> term(X), [*], brackets(Y).
term(X) --> brackets(X).

brackets(X) --> ['('], expr(X), [')'].
brackets(X) --> num(X).
brackets(X) --> identifier(X).

identifier(t_id(X)) -->[X],{X \= true}, {X \= false}, {atom(X)}.
num(t_num(X)) --> [X], {number(X)}.

% Boolean Operators
boolean_operator(t_bool_op_and(and))  --> [and].
boolean_operator(t_bool_op_or(or))  --> [or].

% Boolean Operations
bool(false)-->[false].
bool(true)--> [true].
bool(X) --> identifier(X).
bool(t_notbool(not, X))--> [not], bool(X).
bool(t_bool(X,Y,Z))--> expr(X), comparison_operator(Y), expr(Z).
bool(t_bool_operation(X, Y, Z)) --> bool(X), boolean_operator(Y), bool(Z).

% Need to redefine not equal to operator
comparison_operator(t_comp_op(>)) --> [>].
comparison_operator(t_comp_op(<)) --> [<].
comparison_operator(t_comp_op(==)) --> [==].
comparison_operator(t_comp_op(<=)) --> [<=].
comparison_operator(t_comp_op(>=)) --> [>=].
comparison_operator(t_comp_op(=\=)) --> ["!="].

% Ternary Operation
ternary_op(t_ternary(X, Y, Z)) --> bool(X), [?], expr(Y), [:], expr(Z).

% Declaration statements
declaration(t_declaration_bool_assign(X, Y)) --> [boolean], identifier(X), [=], bool(Y).
declaration(t_declaration_bool_assign(X)) --> [boolean], identifier(X).
declaration(t_declaration_str_assign(X, Y)) --> [string], identifier(X), [=], [Y], {string(Y)}.
declaration(t_declaration_str_assign(X)) --> [string], identifier(X).
declaration(t_declaration_num_assign(X, Y)) --> [num], identifier(X), [=], expr(Y).
declaration(t_declaration_num_assign(X)) --> [num], identifier(X).
declaration(t_declaration_num_assign_ternary(X, Y)) --> [num], identifier(X), [=], ternary_op(Y).

% Assignment statements
assignment(t_assignment_bool(X, Y)) --> identifier(X), [=], bool(Y).
assignment(t_assignment_str(X, Y)) --> identifier(X), [=], ['"'], [Y], ['"'].
assignment(t_assignment_num_assign(X, Y)) --> identifier(X), [=], expr(Y).
assignment(t_assignment_num_assign_ternary(X, Y)) --> identifier(X), [=], ternary_op(Y).

% Need to implement print statement
eprintv(t_print()) --> [].
eprintv(X) --> [,], printv(X).
printv(t_print(X, Y)) --> [X], {string(X)}, eprintv(Y).
printv(t_print_id(X, Y)) --> identifier(X), eprintv(Y).

% if else statements
if_stmt(t_ifstmt(X, Y, Z)) --> [if], ['('], bool(X), [')'], ['{'], command(Y), ['}'], elif_stmt(Z).

elif_stmt(t_elifstmt(X, Y, Z)) --> [elif], ['('], bool(X), [')'], ['{'], command(Y), ['}'], elif_stmt(Z).
elif_stmt(t_goto_else_stmt(X)) --> else_stmt(X).

else_stmt(t_elsestmt(X)) --> [else], ['{'], command(X), ['}'].
else_stmt(t_elsestmt()) --> [].

% for loops
conventional_for(t_conventional_for(A,B,C,D,E,F)) --> [for], ['('], identifier(A), [=], expr(B), [;], 
    identifier(A), comparison_operator(C), expr(D), [;], 
    identifier(A), [=], expr(E), [')'], ['{'], command(F), ['}'].

new_for(t_new_for(A,B,C,D)) --> [for], identifier(A), [in], 
    [range], ['('], expr(B), [,], expr(C), [')'], ['{'], command(D), ['}'].

% General Statements and While loop
statement(t_statement_declaration(X)) --> declaration(X).
statement(t_statement_assign(X)) --> assignment(X).
statement(t_statement_print(X)) --> [print], ['('] , printv(X), [')'].
statement(t_statement_ifelse(X)) --> if_stmt(X).
statement(t_statement_while(X, Y)) --> [while], ['('], bool(X), [')'], ['{'], command(Y), ['}'].
statement(t_statement_for(X)) --> conventional_for(X).
statement(t_statement_for(X)) --> new_for(X).

% Command List and single command is called statement.
command(t_command(X, Y)) --> statement(X), command(Y).
command(t_command()) --> [].

% Block.
block(t_block(X))-->command(X).

% Program entr point. Will take input as list of tokens and generate parse tree.
program(t_program(X))-->block(X).