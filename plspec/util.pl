
success_failure_total(Success, Failure, Total) :-
    aggregate_all(count, plspec:success(_, _), Success),
    aggregate_all(count, plspec:failure(_, _, _), Failure),
    Total is Success + Failure.

spec_path(What, Test, Relative:LineNumber) :-
    clause(plspec:spec(What, Test, _), _, Ref),
    clause_property(Ref, source(Path)),
    clause_property(Ref, line_count(LineNumber)),
    path_relative(Path, Relative).

path_relative(Path, Relative) :-
    working_directory(Dir, Dir),
    concat(Dir, Relative, Path).
