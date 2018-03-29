
library_directory('..').

:- use_module(library(plspec)).

:- ['./main.pl'].

:- describe(add1/2).

it('should add one to the first argument') :-
    add1(0, 1).

:- end(add1/2).

:- describe(sub1/2).

it('should add one to the first argument') :-
    sub1(1, 0).

:- end(sub1/2).
