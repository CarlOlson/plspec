
:- use_module(main).
:- use_module('../plspec').

:- describe(add1/2).

it('should add one to the first argument') :-
    add1(0, 1).

:- end(add1/2).

:- describe(sub1/2).

it('should subtract one from first argument') :-
    sub1(1, 0).

:- end(sub1/2).
