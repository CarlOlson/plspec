
:- module(plspec, [describe/1, run_specs/0]).

:- multifile spec/3.
:- multifile describe/1.

:- dynamic spec/3.
:- dynamic describe/1.
:- dynamic describing/1.

:- multifile system:term_expansion/2.

:- style_check(-singleton).

$trace :- trace.
$notrace :- notrace.

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

run_specs :-
    forall(
        spec(What, Test, Body),
        run_spec(What, Test, Body)
    ),
    success_failure_total(SuccessCount, _, Total),
    format("~d of ~d Passed", [SuccessCount, Total]),
    cleanup.

run_spec(What, Test, Body) :-
    current_prolog_flag(debug, Debugging),
    asserta(plspec:under_test(What, Test)),
    (  call(($trace, Body, $notrace))
    -> assert(plspec:success(What, Test))
    ;  assert(plspec:failure(What, Test, _))
    ),
    retractall(plspec:under_test(_, _)),
    set_prolog_flag(debug, Debugging).

cleanup :-
    retractall(plspec:failure(_, _, _)),
    retractall(plspec:success(_, _)).

success_failure_total(Success, Failure, Total) :-
    ( predicate_property(success(_, _), number_of_clauses(Success))
    ; Success = 0 ),
    ( predicate_property(failure(_, _, _), number_of_clauses(Failure))
    ; Failure = 0 ),
    Total is Success + Failure,
    !.

term_expansion(it(Test) :- Body, spec(What, Test, Body)) :-
    describing(What),
    !.

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

:- load_files(['./plspec_test', './plspec_spec']).
:- initialization run_tests.
:- initialization run_specs.
