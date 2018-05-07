
success_failure_total(Success, Failure, Total) :-
    aggregate_all(count, plspec:success(_, _), Success),
    aggregate_all(count, plspec:failure(_, _, _), Failure),
    Total is Success + Failure.

spec_path(What, Test, Relative:LineNumber) :-
    working_directory(Dir, Dir),
    clause(plspec:spec(What, Test, _), _, Ref),
    clause_property(Ref, source(FileName)),
    clause_property(Ref, line_count(LineNumber)),
    concat(Dir, Relative, FileName).
