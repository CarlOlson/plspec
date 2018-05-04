
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
    print_success,
    print_failure,
    cleanup.

run_spec(What, Test, Body) :-
    current_prolog_flag(debug, Debugging),
    asserta(plspec:under_test(What, Test)),
    spec_eval(Body),
    retractall(plspec:under_test(_, _)),
    set_prolog_flag(debug, Debugging).

spec_eval(Body) :-
    catch( call(($trace, Body, $notrace)),
           Error,
           (spec_catch_error(Error), false)
         )
    -> assert_success
    ;  $notrace.

spec_catch_error(Error) :-
    $notrace,
    assert_failure([error(Error)]).

cleanup :-
    retractall(plspec:failure(_, _, _)),
    retractall(plspec:success(_, _)).

assert_success :-
    plspec:under_test(What, Test),
    assert(plspec:success(What, Test)).

assert_failure(NewOptions) :-
    plspec:under_test(What, Test),
    (  plspec:failure(What, Test, OldOptions)
    -> retractall(plspec:failure(What, Test, _))
    ;  OldOptions = []
    ),
    merge_options(NewOptions, OldOptions, Options),
    assert(plspec:failure(What, Test, Options)).

print_success :-
    forall(plspec:success(What, Test),
           format("PASSED: ~p ~p~n", [What, Test])
          ).

print_failure :-
    forall(plspec:failure(What, Test, Params),
           (
               format("FAILED: ~p ~p~n", [What, Test]),

               (  option(error(Error), Params)
               -> print_message(error, Error)
               ),

               option(backtrace(Backtrace), Params, []),
               print_backtrace(Backtrace)
           )
          ).

print_backtrace([]).
print_backtrace([frame(Depth, Clause, Term) | Rest]) :-
    format("      [~d] ~p~n", [Depth, Term]),
    print_backtrace(Rest).

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
    \+ plspec:failure(_, _, _),
    prolog_frame_attribute(Frame, goal, Goal),
    get_prolog_backtrace(-1, Backtrace, [frame(Frame)]),
    assert_failure([ backtrace(Backtrace), goal(Goal) ]).
user:prolog_trace_interception(_, _, _, continue) :-
    plspec:under_test(What, Test).
