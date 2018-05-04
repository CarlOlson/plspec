
failure_with_stack :-
    !,
    1 == 0.

:- describe(describe/1).

it('should be able to test itself') :-
    A is 2,
    A = 2.

it('should fail and print a stack') :-
    failure_with_stack.

it('should fail and print an error') :-
    prefixr("yolo", _).

:- end(describe/1).
