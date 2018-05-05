
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
