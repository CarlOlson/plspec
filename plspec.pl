
:- module(plspec, [describe/1, end/1, run_specs/0]).

:- dynamic describing/1.
:- dynamic failure/3.
:- dynamic success/2.

:- multifile spec/3.
:- multifile user:term_expansion/2.
:- multifile user:prolog_trace_interception/4.

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
    forall(plspec:success(What, Test),
           format("PASSED: ~p ~p~n", [What, Test])
          ),
    forall(plspec:failure(What, Test, _),
           format("FAILED: ~p ~p~n", [What, Test])
          ),
    cleanup.

run_spec(What, Test, Body) :-
    current_prolog_flag(debug, Debugging),
    asserta(plspec:under_test(What, Test)),
    (  call(($trace, Body, $notrace))
    -> assert(plspec:success(What, Test))
    ;  $notrace
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

error_format(Error, Format, Args) :-
    (
        source_location(File, Line);
        [File, Line] = [unknown, 0]
    ),
    format(string(Message), Format, Args),
    format(string(Error), "~nError: ~s:~d:~n\t~s", [File, Line, Message]).

check(describe, What, Error) :-
    \+ ground(What),
    error_format(Error, "Term must be ground", []).
check(end, What, Error) :-
    \+ ground(What),
    error_format(Error, "Term must be ground", []).
check(end, What, Error) :-
    \+ describing(What),
    error_format(Error, "Not in spec ~p", [What]).

user:term_expansion(it(Test) :- Body, plspec:spec(What, Test, Body)) :-
    describing(What),
    !.

user:prolog_trace_interception(fail, Frame, _, fail) :-
    plspec:under_test(What, Test),
    prolog_frame_attribute(Frame, goal, Goal),
    get_prolog_backtrace(-1, Backtrace, [frame(Frame)]),
    retractall(plspec:failure(What, Test, _)),
    assert(plspec:failure(What, Test,
                          [ backtrace(Backtrace),
                            goal(Goal)
                          ])).
user:prolog_trace_interception(_, _, _, continue) :-
    plspec:under_test(What, Test).
