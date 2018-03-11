
:- module(plspec, [describe/1, spec/2]).

:- multifile spec/2.
:- multifile describe/1.

:- dynamic spec/2.
:- dynamic describe/1.
:- dynamic describing/1.

:- style_check(-singleton).

describe(What) :-
    check(describe, What, Error),
    write(Error),
    halt.
describe(What) :-
    asserta(describing(What)).

end(What) :-
    check(end, What, Error),
    write(Error),
    halt.
end(What) :-
    retract(describing(What)).

check(describe, What, Error) :-
    \+ ground(What),
    error_format(Error, "Term must be ground", []).
check(end, What, Error) :-
    \+ ground(What),
    error_format(Error, "Term must be ground", []).
check(end, What, Error) :-
    \+ describing(What),
    error_format(Error, "Not in spec ~p", [What]).

error_format(Error, Format, Args) :-
    (
        source_location(File, Line);
        [File, Line] = [unknown, 0]
    ),
    format(string(Message), Format, Args),
    format(string(Error), "~nError: ~s:~d:~n\t~s", [File, Line, Message]).

:- describe(describe/1).
:- end(describe/1).

:- begin_tests(plspec).

test('check(describe) should error on nonground terms', [nondet]) :-
    check(describe, Var, Error).

test('check(end) should error on nonground terms', [nondet]) :-
    check(end, Var, Error).

test('check(end) should error on on wrong spec', [nondet]) :-
    check(end, wrong_spec, Error).

test('describe/1 and end/1 should assert/retract describing/1') :-
    Spec = current_spec,
    \+ describing(Spec),
    describe(Spec),
    describing(Spec),
    end(Spec),
    \+ describing(Spec).

:- end_tests(plspec).

:- initialization run_tests.
