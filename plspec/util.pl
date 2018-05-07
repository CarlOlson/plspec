
success_failure_total(Success, Failure, Total) :-
    aggregate_all(count, success(_, _), Success),
    aggregate_all(count, failure(_, _, _), Failure),
    Total is Success + Failure.

spec_path(What, Test, Relative:LineNumber) :-
    working_directory(Dir, Dir),
    clause(plspec:spec(What, Test, Body), _, Ref),
    clause_property(Ref, source(FileName)),
    clause_property(Ref, line_count(LineNumber)),
    concat(Dir, Relative, FileName).
