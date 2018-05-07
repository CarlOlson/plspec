
:- module(main, [add1/2, sub1/2]).

add1(X, Y) :-
    Y is X + 1.

sub1(X, Y) :-
    Y = X.
