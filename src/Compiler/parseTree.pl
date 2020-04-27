% Parse Tree Generator
%:- use_rendering(svgtree).

%:- use_module(library(tabling)).
:- table expr_op/3, term/3, bool/3.

% Update Environment
update(t_id(K), Type, Env, FinalEnv) :- updte(K, Type, Env, FinalEnv).
updte(K, Type, [], [(K,Type)]).
updte(K, Type, [(K, _)|T], [(K, Type)|T]).
updte(K, Type, [H|T], [H|R]) :- H \= (K,_), updte(K, Type, T, R).


% Lookup Value in Environment
lookup(t_id(K), Type, Env) :- look_up(K, Type, Env).
look_up(K, _Type, []) :- write("Variable "), write(K), write(" not defined properly \n"), abort.
look_up(K, Type, [(K,Type)|_T]).
look_up(K1, Type, [(K2,_T2)|T]) :- K1 \= K2, look_up(K1, Type, T).


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
bool(t_bool_operation(X,Y,Z)) --> bool(X), boolean_operator(Y), boolean(Z).
bool(X) --> boolean(X).
boolean(t_bool(X,Y,Z)) --> expr(X), comparison_operator(Y), expr(Z).
boolean(t_notbool(not, X)) --> [not], boolean(X).
boolean(X) --> identifier(X).
boolean(true) --> [true].
boolean(false) --> [false].
boolean(X) --> brkt_bool(X).
brkt_bool(X)-->['('], bool(X), [')'].

% Need to redefine not equal to operator
comparison_operator(t_comp_op(>)) --> [>].
comparison_operator(t_comp_op(<)) --> [<].
comparison_operator(t_comp_op(==)) --> [==].
comparison_operator(t_comp_op(<=)) --> [<=].
comparison_operator(t_comp_op(>=)) --> [>=].
comparison_operator(t_comp_op(=\=)) --> ["!="].

% Ternary Operation
ternary_op(t_ternary(X, Y, Z)) --> bool(X), [?], expr(Y), [:], expr(Z).

% String Manipulation
string_type(t_string_concat_id(X)) --> identifier(X).
string_type(t_string_concat_str(X)) --> [X], {string(X)}.
string_add(t_string_concat(X, Y)) --> string_type(X), [+], string_type(Y).

% Declaration statements
declaration(Env, FinalEnv, t_declaration_bool_assign(X, Y)) --> [boolean], identifier(X), [=], bool(Y), {update(X, bool, Env, FinalEnv)}.
declaration(Env, FinalEnv, t_declaration_bool_assign(X)) --> [boolean], identifier(X), {update(X, bool, Env, FinalEnv)}.
declaration(Env, FinalEnv, t_declaration_str_assign(X, Y)) --> [string], identifier(X), [=], [Y], {string(Y)}, {update(X, str, Env, FinalEnv)}.
declaration(Env, FinalEnv, t_declaration_str_assign(X)) --> [string], identifier(X),{update(X, str, Env, FinalEnv)}.
declaration(Env, FinalEnv, t_declaration_str_assign_concat(X, Y)) --> [string], identifier(X), [=], string_add(Y), {update(X, str, Env, FinalEnv)}.
declaration(Env, FinalEnv, t_declaration_num_assign(X, Y)) --> [num], identifier(X), [=], expr(Y), {update(X, num, Env, FinalEnv)}.
declaration(Env, FinalEnv, t_declaration_num_assign(X)) --> [num], identifier(X), {update(X, num, Env, FinalEnv)}.
declaration(Env, FinalEnv, t_declaration_num_assign_ternary(X, Y)) --> [num], identifier(X), [=], ternary_op(Y), {update(X, num, Env, FinalEnv)}.

% Assignment statements
assignment(Env, Env, t_assignment_num_assign(X, Y)) --> identifier(X), [=], expr(Y), {lookup(X, num, Env)}.
assignment(Env, Env, t_assignment_num_assign_ternary(X, Y)) --> identifier(X), [=], ternary_op(Y) , {lookup(X, num, Env)}.
assignment(Env, Env, t_assignment_bool(X, Y)) --> identifier(X), [=], bool(Y), {lookup(X, bool, Env)}.
assignment(Env, Env, t_assignment_str(X, Y)) --> identifier(X), [=], [Y], {string(Y)}, {lookup(X, str, Env)}.
assignment(Env, Env, t_assignment_str_concat(X, Y)) --> identifier(X), [=], string_add(Y), {lookup(X, str, Env)}.


% Print statements
print_lookup(X, Env, true):- lookup(X, str, Env); lookup(X, bool, Env).
print_statement_list(_Env, t_print()) --> [].
print_statement_list(Env, X) --> [,], print_statement(Env, X).
print_statement(Env, t_print(X, Y)) --> [X], {string(X)}, print_statement_list(Env, Y).
print_statement(Env, t_print_id(X, Y)) --> identifier(X), {print_lookup(X, Env, true)}, print_statement_list(Env, Y).
print_statement(Env, t_print_expr(X, Y)) --> expr(X), {\+print_lookup(X, Env, true)}, print_statement_list(Env, Y).

% if else statements
if_stmt(Env, t_ifstmt(X, Y, Z)) --> [if], ['('], bool(X), [')'], ['{'], command(Env, _, Y), ['}'], elif_stmt(Env, Z).

elif_stmt(Env, t_elifstmt(X, Y, Z)) --> [elif], ['('], bool(X), [')'], ['{'], command(Env, _, Y), ['}'], elif_stmt(Env, Z).
elif_stmt(Env, t_goto_else_stmt(X)) --> else_stmt(Env, X).

else_stmt(Env, t_elsestmt(X)) --> [else], ['{'], command(Env, _, X), ['}'].
else_stmt(_, t_elsestmt()) --> [].

% for loops
conventional_for(Env, t_conventional_for(A,B,C,D,E,F)) --> [for], ['('], identifier(A), [=], expr(B), [;],
    identifier(A), comparison_operator(C), expr(D), [;],
    identifier(A), [=], expr(E), [')'], ['{'], command(Env, _, F), ['}'].

new_for(Env, t_new_for(A,B,C,D)) --> [for], identifier(A), [in],
    [range], ['('], expr(B), [,], expr(C), [')'], ['{'], command(Env, _, D), ['}'].

% General Statements and While loop
statement(Env, FinalEnv, t_statement_declaration(X)) --> declaration(Env, FinalEnv, X).
statement(Env, FinalEnv, t_statement_assign(X)) --> assignment(Env, FinalEnv, X).
statement(Env, Env, t_statement_print(X)) --> [print], ['('] , print_statement(Env, X), [')'].
statement(Env, Env, t_statement_ifelse(X)) --> if_stmt(Env, X).
statement(Env, Env, t_statement_while(X, Y)) --> [while], ['('], bool(X), [')'], ['{'], command(Env, _, Y), ['}'].
statement(Env, Env, t_statement_for(X)) --> conventional_for(Env, X).
statement(Env, Env, t_statement_for(X)) --> new_for(Env, X).

% Command List and single command is called statement.
command(Env, FinalEnv, t_command(X, Y)) --> statement(Env, Env1, X), command(Env1, FinalEnv, Y).
command(Env, Env, t_command()) --> [].

% Block.
block(t_block(X))-->command([], _, X).

% Program entr point. Will take input as list of tokens and generate parse tree.
program(t_program(X))-->block(X).
