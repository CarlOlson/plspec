
extensions_call(Pred) :-
    Option =.. [Pred, Body],
    forall(
        (
            plspec:extension(_, Options),
            option(Option, Options)
        ),
        catch(Body, _Error, true)
    ).
