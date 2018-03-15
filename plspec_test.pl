
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

test('run_spec/3 should assert success predicates',
     [ cleanup(cleanup) ]) :-
    run_spec(atopic, atest, true),
    plspec:success(atopic, atest).

test('run_spec/3 should not fail on test failure',
     [ cleanup(cleanup) ]) :-
    run_spec(atopic, atest, false),
    \+ plspec:success(atopic, atest).

test('run_spec/3 should assert failure predicates',
     [ cleanup(cleanup) ]) :-
    run_spec(atopic, atest, false),
    plspec:failure(atopic, atest, _).

:- end_tests(plspec_test).
