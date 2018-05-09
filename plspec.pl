
:- module(plspec, [describe/1, end/1, run_specs/0]).

:- include(plspec/util).
:- include(plspec/extension).

:- dynamic describing/1.
:- dynamic failure/3.
:- dynamic success/2.
:- dynamic extension/2.

:- multifile spec/3.
:- multifile extension/2.
:- multifile user:term_expansion/2.
:- multifile user:prolog_trace_interception/4.

:- use_module(plspec/print).

'$trace' :- trace.
'$notrace' :- notrace.

describe(What) :-
    asserta(describing(What)).

end(What) :-
    retract(describing(What)).

run_specs :-
    call_extensions(before_all),
    forall(
        spec(What, Test, Body),
        run_spec(What, Test, Body)
    ),
    call_extensions(after_all),
    cleanup.

run_spec(What, Test, Body) :-
    setup_call_cleanup(
        (
            current_prolog_flag(debug, Debugging),
            asserta(plspec:under_test(What, Test))
        ),
        (
            call_extensions(before_each),
            spec_eval(Body),
            call_extensions(after_each)
        ),
        (
            retractall(plspec:under_test(_, _)),
            set_prolog_flag(debug, Debugging)
        )
    ).

spec_eval(Body) :-
    catch(
        setup_call_cleanup('$trace', (Body, !), '$notrace'),
        Error,
        (
            assert_failure([error(Error)]),
            false
        )
    )
    -> assert_success
    ;  true.

cleanup :-
    retractall(plspec:failure(_, _, _)),
    retractall(plspec:success(_, _)).

assert_success :-
    plspec:under_test(What, Test),
    retractall(plspec:failure(What, Test, _)),
    assert(plspec:success(What, Test)).

assert_failure(NewOptions) :-
    plspec:under_test(What, Test),
    (
        plspec:failure(What, Test, OldOptions)
    ;   OldOptions = []
    ),
    retractall(plspec:failure(What, Test, _)),
    merge_options(NewOptions, OldOptions, Options),
    assert(plspec:failure(What, Test, Options)).

user:term_expansion(it(Test) :- Body, plspec:spec(What, Test, Body)) :-
    describing(What).

user:prolog_trace_interception(fail, Frame, _, fail) :-
    under_test(What, Test),
    \+ plspec:failure(What, Test, _),
    prolog_frame_attribute(Frame, goal, Goal),
    get_prolog_backtrace(-1, Backtrace, [frame(Frame)]),
    assert_failure([ backtrace(Backtrace), goal(Goal) ]).
user:prolog_trace_interception(_, _, _, continue) :-
    under_test(_, _).
