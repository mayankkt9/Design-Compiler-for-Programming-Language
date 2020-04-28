% Runtime Semantics

% Update Environment
update(t_id(K), V, Type, Env, FinalEnv) :- update(K, V, Type, Env, FinalEnv).
update(K, V, Type, [], [(K, V, Type)]) :- K \= t_id(_).
update(K, V, Type, [(K, _, _)|T], [(K, V, Type)|T]) :- K \= t_id(_).
update(K, V, Type, [H|T], [H|R]) :- K \= t_id(_), H \= (K,_,_), update(K, V, Type, T, R).



% Lookup Value in Environment
lookup(t_id(K), Env, V, Type) :- lookup(K, Env, V, Type).
lookup(K, [(K,V,Type)|_], V, Type) :- K \= t_id(_).
lookup(K, [_|T], V, Type) :- K \= t_id(_), lookup(K, T, V, Type).



% Check if an identifier is present in env.
check_present(t_id(K),Env) :- check_present(K, Env).
check_present(K,[(K,_,_)|_]).
check_present(K,[(H,_,_)|T]) :- K \= H, check_present(K,T).

% Evaluate Expression
eval_expr(t_assign(t_id(X), Y), Env, FinalEnv, Val):- check_present(X, Env),
    eval_expr(Y, Env, Env1, Val), update(X, Val, num, Env1, FinalEnv).

eval_expr(t_assign(t_id(X), _Y), Env, _FinalEnv, _Val):- \+check_present(X, Env),
    write("Variable not initialised. Please check."),nl, abort.

eval_expr(t_assign(t_id(X), _Y), Env, _FinalEnv, _Val):- lookup(X, Env, _, Type),
    Type \= num, write("This operation can only be perfomed on num type of variable. Please check."),nl, abort.

eval_expr(t_add(X, Y), Env, FinalEnv, Val):- eval_expr(X, Env, Env1, V1),
                                             eval_expr(Y, Env1, FinalEnv, V2),
                                             Val is V1 + V2.

eval_expr(t_sub(X, Y), Env, FinalEnv, Val):- eval_expr(X, Env, Env1, V1),
                                             eval_expr(Y, Env1, FinalEnv, V2),
                                             Val is V1 - V2.

eval_expr(t_div(X, Y), Env, FinalEnv, Val):- eval_expr(X, Env, Env1, V1),
                                             eval_expr(Y, Env1, FinalEnv, V2),
    							   			 Val is V1 / V2.

eval_expr(t_mul(X, Y), Env, FinalEnv, Val):- eval_expr(X, Env, Env1, V1),
                                             eval_expr(Y, Env1, FinalEnv, V2),
    							   			 Val is V1 * V2.

eval_expr(t_num(X), Env, Env, X).

eval_expr(t_id(X), Env, Env, Val):- check_present(X, Env), lookup(X, Env, Val, num).

eval_expr(t_id(X), Env, Env, _Val):- \+check_present(X, Env), write("Variable not initialised. Please check."),nl, abort.

eval_expr(t_id(X), Env, Env, Val):- lookup(X, Env, Val, Type), Type \= num,
    write("This operation can only be perfomed on num type of variable. Please check."),nl, abort.



% Evaluate Boolean Expression
not(true, false).
not(false, true).

eval_bool(false, Env, Env, false).
eval_bool(true, Env, Env, true).
eval_bool(t_id(X), Env, Env, Val) :- check_present(X, Env), lookup(X, Env, Val, bool).
eval_bool(t_id(X), Env, Env, Val):- lookup(X, Env, Val, Type), Type \= bool,
    write("This operation can only be perfomed on boolean type of variable. Please check."), nl, abort.

eval_bool(t_id(X), Env, Env, _Val):- \+check_present(X, Env), write("Variable not initialised. Please check."), 
    nl, abort.

eval_bool(t_notbool(not, X), Env, FinalEnv, Val) :- eval_bool(X, Env, FinalEnv, V1), not(V1, Val).


eval_bool(t_bool_operation(X, Y, Z), Env, FinalEnv, Val) :- eval_bool(X, Env, Env1, V1),
    eval_bool(Z, Env1, FinalEnv, V2),
    eval_bool_operator(Y, V1, V2, Val).

eval_bool(t_bool(X, Y, Z), Env, FinalEnv, Val):- eval_expr(X, Env, Env1,V1), 
    eval_expr(Z, Env1, FinalEnv,V2),
    eval_compare(Y, V1, V2, Val).

eval_compare(t_comp_op(>), V1, V2, true):- V1 > V2.
eval_compare(t_comp_op(>), V1, V2, false):- V1 =< V2.

eval_compare(t_comp_op(<), V1, V2, true):- V1 < V2.
eval_compare(t_comp_op(<), V1, V2, false):- V1 >= V2.

eval_compare(t_comp_op(==), V1, V2, true):- V1 =:= V2.
eval_compare(t_comp_op(==), V1, V2, false):- V1 =\= V2.

eval_compare(t_comp_op(<=), V1, V2, true):- V1 =< V2.
eval_compare(t_comp_op(<=), V1, V2, false):- V1 > V2.
                
eval_compare(t_comp_op(>=), V1, V2, true):- V1 >= V2.
eval_compare(t_comp_op(>=), V1, V2, false):- V1 < V2.

eval_compare(t_comp_op(=\=), V1, V2, false):- V1 =:= V2.
eval_compare(t_comp_op(=\=), V1, V2, true):- V1 =\= V2.

% And OR boolean operations
eval_bool_operator(t_bool_op_and(and),false,true,false).
eval_bool_operator(t_bool_op_and(and),false,false,false).
eval_bool_operator(t_bool_op_and(and),true,false,false).
eval_bool_operator(t_bool_op_and(and),true,true,true).
   
eval_bool_operator(t_bool_op_or(or),false,true,true).
eval_bool_operator(t_bool_op_or(or),false,false,false).
eval_bool_operator(t_bool_op_or(or),true,false,true).
eval_bool_operator(t_bool_op_or(or),true,true,true).



% Evaluate Ternary Statement
eval_ternary(t_ternary(X, Y, _), Env, FinalEnv, Val) :- eval_bool(X, Env, Env1, true),
    eval_expr(Y, Env1, FinalEnv, Val).

eval_ternary(t_ternary(X, _, Z), Env, FinalEnv, Val) :- eval_bool(X, Env, Env1, false),
    eval_expr(Z, Env1, FinalEnv, Val).



% Evaluate String Manipulation
eval_string_type(t_string_concat_id(X), Env, V):- lookup(X, Env, V, str).
eval_string_type(t_string_concat_id(X), Env, _V):- \+check_present(X, Env), 
    write("Variable not initialised. Please check."), nl, abort.
eval_string_type(t_string_concat_id(X), Env, _V):- lookup(X, Env, _Val, Type), 
    Type \= str, 
    write("This operation can only be perfomed on string type of variable. Please check."), 
    nl, abort.
eval_string_type(t_string_concat_str(X), _Env, X).
eval_string_concat(t_string_concat(X, Y), Env, Env, Val) :- eval_string_type(X, Env, V1), 
    eval_string_type(Y, Env, V2), string_concat(V1, V2, Val).



% Evaluate Declaration Statements
eval_declaration(t_declaration_bool_assign(t_id(X),Y), Env, FinalEnv) :- 
    eval_bool(Y, Env, Env1, Val), update(X, Val, bool, Env1 , FinalEnv).

eval_declaration(t_declaration_bool_assign(t_id(X)), Env, FinalEnv) :- 
    update(X, false, bool, Env , FinalEnv).

eval_declaration(t_declaration_str_assign(t_id(X),Y), Env, FinalEnv) :- 
    update(X, Y, str, Env , FinalEnv).

eval_declaration(t_declaration_str_assign(t_id(X)), Env, FinalEnv) :- 
    update(X, "", str, Env , FinalEnv).

eval_declaration(t_declaration_str_assign_concat(X, Y), Env, FinalEnv) :- 
    eval_string_concat(Y, Env, Env1, Val),
    update(X, Val, str, Env1, FinalEnv).

eval_declaration(t_declaration_num_assign(t_id(X),Y), Env, FinalEnv) :- 
    eval_expr(Y, Env, Env1, Val), update(X, Val, num, Env1 , FinalEnv).

eval_declaration(t_declaration_num_assign_ternary(t_id(X), Y), Env, FinalEnv) :- 
    eval_ternary(Y, Env, Env1, Val), update(X, Val, num, Env1 , FinalEnv).

eval_declaration(t_declaration_num_assign(t_id(X)), Env, FinalEnv) :- 
    update(X, 0, num, Env, FinalEnv).

eval_declaration(t_declaration_stack_assign(t_id(X)), Env, FinalEnv) :- 
    update(X, [], stack, Env, FinalEnv).

eval_declaration(t_declaration_stack_assign(t_id(X), Y), Env, FinalEnv) :- 
    update(X, Y, stack, Env, FinalEnv).


% Evaluate assign statements
/*
eval_assignment(t_assignment_bool(t_id(X), _Y), Env, _FinalEnv) :- \+check_present(X, Env), 
    write("Variable not initialised. Please check."),nl, fail.
*/
eval_assignment(t_assignment_bool(t_id(X), Y), Env, FinalEnv) :- 
    %lookup(X, Env, _, bool), 
    eval_bool(Y, Env, Env1, Val),
    update(X, Val, bool, Env1, FinalEnv).
/*
eval_assignment(t_assignment_bool(t_id(X), _Y), Env, _FinalEnv) :- lookup(X, Env, _, Type), Type \= bool,
    write("Cannot assign this value to non boolean type of variable"),nl, fail.

eval_assignment(t_assignment_str(t_id(X), _Y), Env, _FinalEnv) :- \+check_present(X, Env), 
    write("Variable not initialised. Please check."),nl, fail.
*/
eval_assignment(t_assignment_str(t_id(X), Y), Env, FinalEnv) :- 
    %lookup(X, Env, _, str), 
    update(X, Y, str, Env, FinalEnv).

eval_assignment(t_assignment_str_concat(X, Y), Env, FinalEnv) :- 
    eval_string_concat(Y, Env, Env1, Val),
    update(X, Val, str, Env1 , FinalEnv).

/*
eval_assignment(t_assignment_str(t_id(X), _Y), Env, _FinalEnv) :- lookup(X, Env, _, Type), 
	Type \= str,
    write("Cannot assign this value to non string type of variable"),nl.

eval_assignment(t_assignment_num_assign(t_id(X), _Y), Env, _FinalEnv) :- 
	\+check_present(X, Env), 
    write("Variable not initialised. Please check."),nl, fail.

eval_assignment(t_assignment_num_assign_ternary(t_id(X), _Y), Env, _FinalEnv) :- 
	\+check_present(X, Env), 
    write("Variable not initialised. Please check."),nl, fail.
*/
eval_assignment(t_assignment_num_assign(t_id(X), Y), Env, FinalEnv) :- 
	%lookup(X, Env, _, num), 
    eval_expr(Y, Env, Env1, Val), 
	update(X, Val, num, Env1, FinalEnv).

eval_assignment(t_assignment_num_assign_ternary(t_id(X), Y), Env, FinalEnv) :- 
	%lookup(X, Env, _, num), 
    eval_ternary(Y, Env, Env1, Val), 
	update(X, Val, num, Env1, FinalEnv).
/*
eval_assignment(t_assignment_num_assign(t_id(X), _Y), Env, _FinalEnv) :- 
	lookup(X, Env, _, Type), 
	Type \= num,
    write("Cannot assign this value to non boolean type of variable"),nl, 
	fail.

eval_assignment(t_assignment_num_assign_ternary(t_id(X), _Y), Env, _FinalEnv) :- 
	lookup(X, Env, _, Type), 
	Type \= num,
    write("Cannot assign this value to non boolean type of variable"),nl, 
	fail.
*/

eval_assignment(t_assignment_stack(t_id(X), Y), Env, FinalEnv) :- 
	update(X, Y, stack, Env, FinalEnv).


% Print Statements
eval_print(t_print(), Env, Env).
eval_print(t_print(X, Y), Env, FinalEnv) :- write(X), eval_print(Y, Env, FinalEnv).
eval_print(t_print_id(X, Y), Env, FinalEnv) :- lookup(X,Env,Val,_), write(Val), eval_print(Y, Env, FinalEnv).
eval_print(t_print_id(X, _), Env, Env) :- \+check_present(X, Env), write("Variable not initialised. Please check.").
eval_print(t_print_expr(X, Y), Env, FinalEnv) :- eval_expr(X, Env, Env1, Val), write(Val), eval_print(Y, Env1, FinalEnv).



% If else statement
eval_ifelse_stmt(t_ifstmt(X, Y, _), Env, FinalEnv) :- eval_bool(X, Env, Env1, true), eval_command(Y, Env1, FinalEnv).
eval_ifelse_stmt(t_ifstmt(X, _, Z), Env, FinalEnv) :- eval_bool(X, Env, Env1, false), eval_ifelse_stmt(Z, Env1, FinalEnv).
eval_ifelse_stmt(t_elifstmt(X, Y, _), Env, FinalEnv) :- eval_bool(X, Env, Env1, true), eval_command(Y, Env1, FinalEnv).
eval_ifelse_stmt(t_elifstmt(X, _, Z), Env, FinalEnv) :- eval_bool(X, Env, Env1, false), eval_ifelse_stmt(Z, Env1, FinalEnv).
eval_ifelse_stmt(t_goto_else_stmt(X), Env, FinalEnv) :- eval_ifelse_stmt(X, Env, FinalEnv).
eval_ifelse_stmt(t_elsestmt(X), Env, FinalEnv) :- eval_command(X, Env, FinalEnv).
eval_ifelse_stmt(t_elsestmt(), Env, Env) :- true.



% While statement
eval_while(t_statement_while(X,Y), Env, FinalEnv):- eval_bool(X, Env, Env1, true), 
    eval_command(Y, Env1, Env2),
    eval_statement(t_statement_while(X,Y), Env2, FinalEnv).

eval_while(t_statement_while(X,_), Env, FinalEnv):- eval_bool(X, Env, FinalEnv, false).



% For loops
eval_for_loop(t_new_for(A,B,C,D), Env, FinalEnv) :- 
    eval_for_loop(t_conventional_for(A,B,t_comp_op(<),C, t_assign(A, t_add(A, t_num(1))),D), Env, FinalEnv).

eval_for_loop(t_conventional_for(A,B,C,D,E,F), Env, FinalEnv) :- eval_expr(B, Env, Env1, Val), 
	update(A, Val, num, Env1, Env2), 
    eval_for_statement(t_conventional_for(A,B,C,D,E,F), Env2, FinalEnv).

eval_for_statement(t_conventional_for(A,B,C,D,E,F), Env, FinalEnv) :- eval_bool(t_bool(A, C, D), Env, Env1, true), 
    eval_command(F, Env1, Env2), 
	eval_expr(E, Env2, Env3, Val), 
    update(A, Val, num, Env3, Env4), 
	eval_for_statement(t_conventional_for(A,B,C,D,E,F), Env4, FinalEnv).

eval_for_statement(t_conventional_for(A,_,C,D,_,_), Env, Env) :- eval_bool(t_bool(A, C, D), Env, _, false).

% Evaluate stack commands
eval_stack(t_push(X, Y), Env, FinalEnv) :- lookup(X, Env, Val, stack), update(X, [Y|Val], stack, Env, FinalEnv).
eval_stack(t_pop(X), Env, FinalEnv) :- lookup(X, Env, [_|Rest], stack), update(X, Rest, stack, Env, FinalEnv).
eval_stack(t_pop(t_id(X)), Env, Env) :- lookup(X, Env, [], stack), write("Stack "), write(X), write(" is empty.").
eval_stack(t_top(X), Env, Env) :- lookup(X, Env, [Top|_], stack), write(Top).
eval_stack(t_top(t_id(X)), Env, Env) :- lookup(X, Env, [], stack), write("Stack "), write(X), write(" is empty.").


% Evaluate Statements
eval_statement(t_statement_declaration(X), Env, FinalEnv) :- eval_declaration(X, Env, FinalEnv).
eval_statement(t_statement_assign(X), Env, FinalEnv) :- eval_assignment(X, Env, FinalEnv).
eval_statement(t_statement_print(X), Env, FinalEnv) :- eval_print(X, Env, FinalEnv).
eval_statement(t_statement_ifelse(X), Env, FinalEnv) :- eval_ifelse_stmt(X, Env, FinalEnv).	
eval_statement(t_statement_while(X, Y), Env, FinalEnv) :- eval_while(t_statement_while(X, Y), Env, FinalEnv).
eval_statement(t_statement_for(X), Env, FinalEnv) :- eval_for_loop(X, Env, FinalEnv).
eval_statement(t_statement_stack(X), Env, FinalEnv) :- eval_stack(X, Env, FinalEnv).



% Evaluate Command
eval_command(t_command(), Env, Env).
eval_command(t_command(X, Y), Env, FinalEnv) :- eval_statement(X, Env, Env1), eval_command(Y, Env1, FinalEnv).



% Evaluate Block
eval_block(t_block(X), Env, FinalEnv):- eval_command(X, Env, FinalEnv).

program_eval(t_program(X), Env):- eval_block(X, [], Env).