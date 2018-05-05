
:- include(plspec).

mock(Pred) :- mock(Pred, true).
mock(Pred, Body) :-
    asserta((Pred :- !, asserta($mock_called(Pred)), call(Body)), Ref),
    asserta($mocks(Ref)).

mock_called(Pred) :-
    $mock_called(Pred).

mock_called_once(Pred) :-
    aggregate_all(count, $mock_called(Pred), 1).

unmockall :-
    forall($mocks(Ref), erase(Ref)),
    retractall($mocks(_)),
    retractall($mock_called(_)).

success_failure_total(Success, Failure, Total) :-
    aggregate_all(count, success(_, _), Success),
    aggregate_all(count, failure(_, _, _), Failure),
    Total is Success + Failure.

:- begin_tests(plspec_test).

failure_with_stack :-
    1 == 0.

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

test('run_specs/0 should print failures',
     [ cleanup((cleanup, unmockall)) ]) :-
    mock(cleanup),
    mock(format(_, _)),

    asserta(plspec:spec(atopic, atest, false)),
    run_specs,
    retract(plspec:spec(atopic, atest, false)),

    mock_called(format(Message, _)),
    concat("FAILED", _, Message).

test('run_specs/0 should print errors',
     [ cleanup((cleanup, unmockall)) ]) :-
    mock(cleanup),
    mock(format(_, _)),
    mock(print_message(_, _)),

    asserta(plspec:spec(atopic, atest, does_not_exist)),
    run_specs,
    retract(plspec:spec(atopic, atest, does_not_exist)),

    mock_called_once(print_message(error, _)).

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

    mock_called_once($trace),
    mock_called_once($notrace),

    plspec:success(atopic, atest).

test('run_spec/3 should only assert one failure',
     [ cleanup(cleanup), true(Fails =:= 1) ]) :-
    run_spec(atopic, atest, plunit_plspec_test:failure_with_stack),
    success_failure_total(0, Fails, _).

test('run_spec/3 should catch errors as failures',
     [cleanup(cleanup), true(Fails =:= 1) ]) :-
    run_spec(atopic, atest, does_not_exist),
    success_failure_total(0, Fails, _).

test('failure information should include failed goal',
     [ cleanup(cleanup), true(Goal == system:false) ]) :-
    run_spec(atopic, atest, false),
    plspec:failure(atopic, atest, Info),
    option(goal(Goal), Info).

test('failure information should include backtrace (starting at goal)',
     [ cleanup(cleanup), true(Pred == system:false/0) ]) :-
    run_spec(atopic, atest, false),
    plspec:failure(atopic, atest, Info),
    option(backtrace(Backtrace), Info),
    Backtrace = [frame(_depth, call(Pred), _trace) | _].

:- end_tests(plspec_test).
