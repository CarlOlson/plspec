
:- include('./plspec').
:- initialization run_tests.

mock(Pred, Body) :-
    asserta((Pred :- !, asserta($mock_called(Pred)), call(Body)), Ref),
    asserta($mocks(Ref)).

mock_called(Pred) :-
    $mock_called(Pred),
    !.

unmockall :-
    forall($mocks(Ref), erase(Ref)),
    retractall($mocks(_)),
    retractall($mock_called(_)).

:- begin_tests(plspec_test).

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

test('cleanup/0 should retract all success/2') :-
    asserta(plspec:success(0, 0)),
    cleanup,
    \+ plspec:success(0, 0).

test('cleanup/0 should retract all failure/3') :-
    asserta(plspec:failure(0, 0, 0)),
    cleanup,
    \+ plspec:failure(0, 0, 0).

test('success_failure_total/3 should count success/2',
     [ true(Count == 1), cleanup(cleanup) ]) :-
    asserta(plspec:success(0, 0)),
    success_failure_total(Count, _, _).

test('success_failure_total/3 should count failure/3',
     [ true(Count == 1), cleanup(cleanup) ]) :-
    asserta(plspec:failure(0, 0, 0)),
    success_failure_total(_, Count, _).

test('success_failure_total/3 should have a total',
     [ true(Count == 2), cleanup(cleanup) ]) :-
    asserta(plspec:success(0, 0)),
    asserta(plspec:failure(0, 0, 0)),
    success_failure_total(_, _, Count).

test('run_specs/0 should call cleanup',
     [ cleanup((cleanup, unmockall)) ]) :-
    mock(cleanup, true),
    mock(format(_, _), true),
    run_specs,
    mock_called(cleanup).

test('run_spec/3 should assert success predicates',
     [ cleanup(cleanup) ]) :-
    run_spec(atopic, atest, true),
    plspec:success(atopic, atest).

test('run_spec/3 should not fail on test failure',
     [ cleanup(cleanup) ]) :-
    run_spec(atopic, atest, false),
    \+ plspec:success(atopic, atest).

test('run_spec/3 should assert failure predicate only once',
     [ cleanup(cleanup), true(FailureClauses =:= 1) ]) :-
    run_spec(atopic, atest, false),
    plspec:failure(atopic, atest, _),

    predicate_property( failure(_, _, _),
                        number_of_clauses(FailureClauses)
                      ).

test('run_spec/3 should not set debug status',
     [ cleanup(cleanup) ]) :-
    current_prolog_flag(debug, false),
    run_spec(atopic, atest, true),
    run_spec(atopic, atest, false),
    current_prolog_flag(debug, false).

test('run_spec/3 should retain debug status',
     [ cleanup((cleanup, nodebug)) ]) :-
    set_prolog_flag(debug, true),
    run_spec(atopic, atest, true),
    run_spec(atopic, atest, false),
    current_prolog_flag(debug, true).

test('run_spec/3 should set under_test/2 while testing',
     [ cleanup(cleanup) ]) :-
    run_spec(atopic, atest, (plspec:under_test(atopic, atest))),
    plspec:success(atopic, atest),
    \+ plspec:under_test(atopic, atest).

test('run_spec/3 should trace current spec',
     [ cleanup((cleanup, unmockall)) ]) :-
    mock($trace, true),
    mock($notrace, true),

    run_spec(atopic, atest, true),

    mock_called($trace),
    mock_called($notrace),

    plspec:success(atopic, atest).

test('failure information should include failed goal',
     [ cleanup(cleanup), true(Goal == system:false) ]) :-
    run_spec(atopic, atest, false),
    plspec:failure(atopic, atest, Info),
    member(goal(Goal), Info),
    !.

test('failure information should include backtrace (starting at goal)',
     [ cleanup(cleanup), true(Pred == system:false/0) ]) :-
    run_spec(atopic, atest, false),
    plspec:failure(atopic, atest, Info),
    member(backtrace(Backtrace), Info),
    !,
    Backtrace = [frame(_depth, call(Pred), _trace) | _].

:- end_tests(plspec_test).
