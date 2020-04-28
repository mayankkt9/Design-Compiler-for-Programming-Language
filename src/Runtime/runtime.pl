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

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% List operations
addAtIndex(_ValToAdd, List, Index, _FinalList) :- length(List, Length), Index >= Length + 2, write("Length of the list is "), write(Length), write(". Please provide correct index."), nl, fail.
addAtIndex(_ValToAdd, _List, Index, _FinalList) :- Index =< 0, write("Please provide index greater than 0."), nl, fail.
addAtIndex(ValToAdd, List, Index, FinalList) :- length(List, Length), Index < Length + 2, Index > 0, addAtIndex(ValToAdd, List, 1, Index, FinalList).
addAtIndex(ValToAdd, List, Iterator, Index, [ValToAdd|List]) :- Iterator =:= Index.
addAtIndex(ValToAdd, [H|T], Iterator, Index, [H|FinalList]) :- Iterator < Index, NextIterator = Iterator + 1, addAtIndex(ValToAdd, T, NextIterator, Index, FinalList).

deleteAtIndex(Index, List, _FinalList) :- length(List, Length), Index > Length, write("Length of the list is "), write(Length), write(". Please provide correct index."), nl, fail.
deleteAtIndex(Index, _List, _FinalVal) :- Index =< 0, write("Please provide index greater than 0."), nl, fail.
deleteAtIndex(Index, List, FinalList) :- length(List, Length), Index =< Length, Index > 0, deleteAtIndex(Index, 1, List, FinalList).
deleteAtIndex(Index, Iterator, [_|T], T) :- Index =:= Iterator.
deleteAtIndex(Index, Iterator, [H|T], [H|FinalList]) :- Iterator < Index, NextIterator = Iterator + 1, deleteAtIndex(Index, NextIterator, T, FinalList).

getAtIndex(Index, List, _Val) :- length(List, Length), Index > Length, write("Length of the list is "), write(Length), write(". Please provide correct index."), nl, fail.
getAtIndex(Index, _List, _Val) :- Index =< 0, write("Please provide index greater than 0."), nl, fail.
getAtIndex(Index, List, Val) :- length(List, Length), Index =< Length, Index > 0, getAtIndex(Index, 1, List, Val).
getAtIndex(Index, Iterator, [Val|_], Val) :- Index =:= Iterator.
getAtIndex(Index, Iterator, [_|T], Val) :- Iterator < Index, NextIterator = Iterator + 1, getAtIndex(Index, NextIterator, T, Val).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

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

eval_expr(t_stack(X), Env, FinalEnv, Val) :- eval_stack_pt(X, Val, Env, FinalEnv).
    
eval_expr(t_queue(X), Env, FinalEnv, Val) :- eval_queue_pt(X, Val, Env, FinalEnv).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

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

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate Ternary Statement
eval_ternary(t_ternary(X, Y, _), Env, FinalEnv, Val) :- eval_bool(X, Env, Env1, true),
    eval_expr(Y, Env1, FinalEnv, Val).

eval_ternary(t_ternary(X, _, Z), Env, FinalEnv, Val) :- eval_bool(X, Env, Env1, false),
    eval_expr(Z, Env1, FinalEnv, Val).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

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

%----------------------------------------------------------------------------------------------------------------------------------------------------------

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

eval_declaration(t_declaration_queue_assign(t_id(X)), Env, FinalEnv) :- 
    update(X, [], queue, Env, FinalEnv).

eval_declaration(t_declaration_queue_assign(t_id(X), Y), Env, FinalEnv) :- 
    update(X, Y, queue, Env, FinalEnv).

eval_declaration(t_declaration_list_assign(t_id(X), Y), Env, FinalEnv) :- 
    update(X, Y, list, Env, FinalEnv).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate assign statements
eval_assignment(t_assignment_bool(t_id(X), Y), Env, FinalEnv) :- 
    eval_bool(Y, Env, Env1, Val),
    update(X, Val, bool, Env1, FinalEnv).

eval_assignment(t_assignment_str(t_id(X), Y), Env, FinalEnv) :- 
    update(X, Y, str, Env, FinalEnv).

eval_assignment(t_assignment_str_concat(X, Y), Env, FinalEnv) :- 
    eval_string_concat(Y, Env, Env1, Val),
    update(X, Val, str, Env1 , FinalEnv).

eval_assignment(t_assignment_num_assign(t_id(X), Y), Env, FinalEnv) :- 
    eval_expr(Y, Env, Env1, Val), 
	update(X, Val, num, Env1, FinalEnv).

eval_assignment(t_assignment_num_assign_ternary(t_id(X), Y), Env, FinalEnv) :- 
    eval_ternary(Y, Env, Env1, Val), 
	update(X, Val, num, Env1, FinalEnv).

eval_assignment(t_assignment_stack(t_id(X), Y), Env, FinalEnv) :- 
	update(X, Y, stack, Env, FinalEnv).

eval_assignment(t_assignment_queue(t_id(X), Y), Env, FinalEnv) :- 
	update(X, Y, queue, Env, FinalEnv).

eval_assignment(t_assignment_list(t_id(X), Y), Env, FinalEnv) :- 
	update(X, Y, list, Env, FinalEnv).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Print Statements
eval_print(t_print(), Env, Env).
eval_print(t_print(X, Y), Env, FinalEnv) :- write(X), eval_print(Y, Env, FinalEnv).
eval_print(t_print_id(X, Y), Env, FinalEnv) :- lookup(X,Env,Val,_), write(Val), eval_print(Y, Env, FinalEnv).
eval_print(t_print_id(X, _), Env, Env) :- \+check_present(X, Env), write("Variable not initialised. Please check.").
eval_print(t_print_expr(X, Y), Env, FinalEnv) :- eval_expr(X, Env, Env1, Val), write(Val), eval_print(Y, Env1, FinalEnv).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% If else statement
eval_ifelse_stmt(t_ifstmt(X, Y, _), Env, FinalEnv) :- eval_bool(X, Env, Env1, true), eval_command(Y, Env1, FinalEnv).
eval_ifelse_stmt(t_ifstmt(X, _, Z), Env, FinalEnv) :- eval_bool(X, Env, Env1, false), eval_ifelse_stmt(Z, Env1, FinalEnv).
eval_ifelse_stmt(t_elifstmt(X, Y, _), Env, FinalEnv) :- eval_bool(X, Env, Env1, true), eval_command(Y, Env1, FinalEnv).
eval_ifelse_stmt(t_elifstmt(X, _, Z), Env, FinalEnv) :- eval_bool(X, Env, Env1, false), eval_ifelse_stmt(Z, Env1, FinalEnv).
eval_ifelse_stmt(t_goto_else_stmt(X), Env, FinalEnv) :- eval_ifelse_stmt(X, Env, FinalEnv).
eval_ifelse_stmt(t_elsestmt(X), Env, FinalEnv) :- eval_command(X, Env, FinalEnv).
eval_ifelse_stmt(t_elsestmt(), Env, Env) :- true.

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% While statement
eval_while(t_statement_while(X,Y), Env, FinalEnv):- eval_bool(X, Env, Env1, true), 
    eval_command(Y, Env1, Env2),
    eval_statement(t_statement_while(X,Y), Env2, FinalEnv).

eval_while(t_statement_while(X,_), Env, FinalEnv):- eval_bool(X, Env, FinalEnv, false).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

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

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate stack commands
eval_stack(t_stack_push(X, Y), Env, FinalEnv) :- eval_expr(Y, Env, Env1, V1), lookup(X, Env1, V2, stack), update(X, [V1|V2], stack, Env1, FinalEnv).

eval_stack(t_stack_pt(X), Env, FinalEnv) :- eval_stack_pt(X, _Val, Env, FinalEnv).
eval_stack_pt(t_stack_pop(X), Val, Env, FinalEnv) :- lookup(X, Env, [Val|Rest], stack), update(X, Rest, stack, Env, FinalEnv).
eval_stack_pt(t_stack_pop(X), _Val, Env, Env) :- lookup(X, Env, [], stack), write("Stack "), write(X), write(" is empty."), abort.
eval_stack_pt(t_stack_top(X), Val, Env, Env) :- lookup(X, Env, [Val|_], stack).
eval_stack_pt(t_stack_top(X), _Val, Env, Env) :- lookup(X, Env, [], stack), write("Stack "), write(X), write(" is empty."), nl.

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate queue commands
eval_queue(t_queue_push(X, Y), Env, FinalEnv) :- eval_expr(Y, Env, Env1, V1), lookup(X, Env1, V2, queue), append(V2, [V1], FinalVal), update(X, FinalVal, queue, Env1, FinalEnv).

eval_queue(t_queue_pt(X), Env, FinalEnv) :- eval_queue_pt(X, _Val, Env, FinalEnv).
eval_queue_pt(t_queue_poll(X), Val, Env, FinalEnv) :- lookup(X, Env, [Val|Rest], queue), update(X, Rest, queue, Env, FinalEnv).
eval_queue_pt(t_queue_poll(X), _Val, Env, Env) :- lookup(X, Env, [], queue), write("Queue "), write(X), write(" is empty."), abort.
eval_queue_pt(t_queue_head(X), Val, Env, Env) :- lookup(X, Env, [Val|_], queue).
eval_queue_pt(t_queue_head(X), _Val, Env, Env) :- lookup(X, Env, [], queue), write("Queue "), write(X), write(" is empty."), nl.

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate list commands
eval_list(t_add(X, t_num(Y)), Env, FinalEnv) :- lookup(X, Env, Val, list), append(Val, [Y], FinalVal), update(X, FinalVal, list, Env, FinalEnv).
eval_list(t_add(X, t_num(ValToAdd), t_num(Index)), Env, FinalEnv) :- lookup(X, Env, Val, list), addAtIndex(ValToAdd, Val, Index, FinalVal), update(X, FinalVal, list, Env, FinalEnv).
eval_list(t_remove(X, t_num(Index)), Env, FinalEnv) :- lookup(X, Env, Val, list), deleteAtIndex(Index, Val, FinalVal), update(X, FinalVal, list, Env, FinalEnv).
eval_list(t_get(X, t_num(Index)), Env, Env) :- lookup(X, Env, List, list), getAtIndex(Index, List, Val), write(Val), nl.

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Method
find_val(A, Val, Type, Env):- lookup(A, Env, Val, Type).
find_val(t_str(A), A, _Type, _Env).
find_val(t_num(A), A, _Type, _Env).    

% Create a local environment for method
form_method_env(t_formal_parameter(), t_actual_parameter(), _Env, NewEnv, NewEnv).
form_method_env(t_formal_parameter(X, Y), t_actual_parameter(A, B), Env, NewEnv, NewFinalEnv) :- 
    find_val(A, Val, Type, Env),
    update(X, Val, Type, NewEnv, NewEnv1),
    form_method_env(Y, B, Env, NewEnv1, NewFinalEnv).

% Evaluate body of method
eval_body(t_body(X), Env, FinalEnv) :- eval_command(X, Env, FinalEnv).

% Return Statement Evaluation
eval_return_statement(t_return(), _Val, _Env).
eval_return_statement(t_return(t_str(X)), X, _Env).
eval_return_statement(t_return(X), Val, Type, Env):- lookup(X, Env, Val, Type).
eval_return_statement(t_return(X), Val, num, Env):- eval_expr(X, Env, _FinalEnv, Val).

% Method Declaration evaluation
eval_method(t_method_declaration(FuncName, Parameters, Body, ReturnType), Env, FinalEnv, _Val, _Type) :- 
    update(FuncName, (Parameters, Body, ReturnType), method, Env, FinalEnv).

% Method Call evaluation
eval_method(t_method_call(MethodName, ActualParameters), Env, Env, Val, Type) :- 
    lookup(MethodName, Env, (FormalParameters, Body, ReturnType), method),
    form_method_env(FormalParameters, ActualParameters, Env, [], FinalMethodEnv),
    eval_body(Body, FinalMethodEnv, ResultingEnv), 
    eval_return_statement(ReturnType, Val, Type, ResultingEnv).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate Statements
eval_statement(t_statement_declaration(X), Env, FinalEnv) :- eval_declaration(X, Env, FinalEnv).
eval_statement(t_statement_assign(X), Env, FinalEnv) :- eval_assignment(X, Env, FinalEnv).
eval_statement(t_statement_print(X), Env, FinalEnv) :- eval_print(X, Env, FinalEnv).
eval_statement(t_statement_ifelse(X), Env, FinalEnv) :- eval_ifelse_stmt(X, Env, FinalEnv).	
eval_statement(t_statement_while(X, Y), Env, FinalEnv) :- eval_while(t_statement_while(X, Y), Env, FinalEnv).
eval_statement(t_statement_for(X), Env, FinalEnv) :- eval_for_loop(X, Env, FinalEnv).
eval_statement(t_statement_stack(X), Env, FinalEnv) :- eval_stack(X, Env, FinalEnv).
eval_statement(t_statement_queue(X), Env, FinalEnv) :- eval_queue(X, Env, FinalEnv).
eval_statement(t_statement_list(X), Env, FinalEnv) :- eval_list(X, Env, FinalEnv).
eval_statement(t_statement_method(X), Env, FinalEnv) :- eval_method(X, Env, FinalEnv, _Val, _Type).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate Command
eval_command(t_command(), Env, Env).
eval_command(t_command(X, Y), Env, FinalEnv) :- eval_statement(X, Env, Env1), eval_command(Y, Env1, FinalEnv).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate Block
eval_block(t_block(X), Env, FinalEnv):- eval_command(X, Env, FinalEnv).

program_eval(t_program(X), Env):- eval_block(X, [], Env).