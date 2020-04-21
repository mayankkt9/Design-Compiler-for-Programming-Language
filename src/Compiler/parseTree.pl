% Parse Tree Generator
%:- use_rendering(svgtree).

:- use_module(library(tabling)).
:- table expr_op/3, term/3, bool/3.


% Need to implement % operator and I = E too.
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


% 4 data types
data_type-->[int].
data_type-->[float].
data_type-->[string].
data_type-->[boolean].


% Boolean Operators
boolean_operator(t_bool_op_and(and))  --> [and].
boolean_operator(t_bool_op_or(or))  --> [or].


% Boolena Operations
bool(false)-->[false].
bool(true)--> [true].
bool(t_notbool(not, X))--> [not], bool(X).
bool(t_bool(X,Y,Z))--> expr(X), comparison_operator(Y), expr(Z).
bool(t_bool_operation(X, Y, Z)) --> bool(X), boolean_operator(Y), bool(Z).


% Need to redefine not equal to operator
comparison_operator(t_comp_op(>)) --> [>].
comparison_operator(t_comp_op(<)) --> [<].
comparison_operator(t_comp_op(==)) --> [==].
comparison_operator(t_comp_op(<=)) --> [<=].
comparison_operator(t_comp_op(>=)) --> [>=].
comparison_operator(t_comp_op('!=')) --> ['!='].


% Ternary Operation
ternary_op(t_ternary(X, Y, Z)) --> bool(X), [?], expr(Y), [:], expr(Z).

% Declaration statements
declaration(t_declaration_bool_assign(X, Y)) --> [boolean], identifier(X), [=], bool(Y).
declaration(t_declaration_str_assign(X, Y)) --> [string], identifier(X), [=], ['"'], [Y], ['"'].
declaration(t_declaration_num_assign(X, Y)) --> [int], identifier(X), [=], expr(Y).
declaration(t_declaration_num_assign_ternary(X, Y)) --> [int], identifier(X), [=], ternary_op(Y).
declaration(t_declaration_num_assign(X, Y)) --> [float], identifier(X), [=], expr(Y).
declaration(t_declaration_num_assign_ternary(X, Y)) --> [float], identifier(X), [=], ternary_op(Y).
