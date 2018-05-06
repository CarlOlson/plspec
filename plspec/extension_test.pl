
extension_cleanup :-
    retractall(plspec:extension(_, _)),
    unmockall.

:- begin_tests(plspec_extension_test).

test("should not error on empty extension",
     [ cleanup(extension_cleanup) ]) :-
    asserta(plspec:extension(null_extension, [])),
    run_specs.

test("should call before_all only once",
     [ cleanup(extension_cleanup) ]) :-
    mock(fn),
    asserta(plspec:extension(simple, [before_all(fn)])),
    run_specs,
    mock_called(fn, [once]).

test("should call after_all only once",
     [ cleanup(extension_cleanup)  ]) :-
    mock(fn),
    asserta(plspec:extension(simple, [after_all(fn)])),
    run_specs,
    mock_called(fn, [once]).

test("should call in order before_all > specs > after_all",
     [ cleanup(extension_cleanup), nondet ]) :-
    mock(before_hook),
    mock(spec_body),
    mock(after_hook),
    mock(format(_, _)),

    asserta(plspec:extension(simple, [
                                 before_all(before_hook),
                                 after_all(after_hook)
                             ])),

    asserta(plspec:spec(subject, test, spec_body)),
    run_specs,
    retractall(plspec:spec(subject, test, spec_body)),

    mocks_called(
        [before_hook, spec_body, after_hook],
        [in_order, once]
    ).

:- end_tests(plspec_extension_test).
