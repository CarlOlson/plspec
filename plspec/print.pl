
extension(print, [after_all((print_summary, print_failure))]).

print_summary :-
    success_failure_total(Success, _, Total),
    format("~d of ~d Passed~n", [Success, Total]).

print_success :-
    forall(
        plspec:success(What, Test),
        format("PASSED: ~p ~s~n", [What, Test])
    ).

print_failure :-
    forall(
        plspec:failure(What, Test, Params),
        (
            (  spec_path(What, Test, Path:Line)
            -> format("FAILED: ~s:~d~n\t~p ~s~n", [Path, Line, What, Test])
            ;  format("FAILED: ~n~t~p ~s~n", [What, Test])
            ),

            (  option(error(Error), Params)
            -> print_message(error, Error)
            ;  true
            ),

            (  option(backtrace(Backtrace), Params)
            -> print_backtrace(Backtrace)
            ;  true
            )
        )
    ).

print_backtrace([]).
print_backtrace([frame(_, _, '<meta-call>'(plspec:_)) | _]).
print_backtrace([frame(Depth, Clause, Term) | Rest]) :-
    format("\t [~d] ~p~n", [Depth, Term]),
    print_backtrace(Rest).
