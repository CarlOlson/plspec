
extensions_call(Pred) :-
    Option =.. [Pred, Body],
    forall(
        (
            plspec:extension(_, Options),
            option(Option, Options)
        ),
        call(Body)
    ).
