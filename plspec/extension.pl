
call_extensions(Hook) :-
    Option =.. [Hook, Body],
    forall(
        extension_option(Extension, Option),
        catch(Body, Error, extension_error(Hook, Extension, Error))
    ).

extension(Extension) :-
    Extension = extension(_, _),
    Extension.

extension_name(Extension, Name) :-
    Extension = extension(Name, _),
    Extension.

extension_options(Extension, Options) :-
    Extension = extension(_, Options),
    Extension.

extension_option(Extension, Option) :-
    extension_options(Extension, Options),
    option(Option, Options).

extension_location(Extension, Path:Line) :-
    predicate_property(Extension, file(Path)),
    predicate_property(Extension, line_count(Line)).

extension_error(Hook, Extension, Error) :-
    print_message(error, Error),
    extension_hook_failed(Extension, Hook).

extension_hook_failed(Extension, Hook) :-
    extension_name(Extension, Name),
    extension_location(Extension, Path:Line),
    path_relative(Path, FileName),
    format(
        "       Extension ~p failed, ~p @ ~s:~p~n",
        [ Name, Hook, FileName, Line ]
    ).
extension_hook_failed(Extension, Hook) :-
    extension_name(Extension, Name),
    format(
        "       Extension ~p failed, ~p~n",
        [ Name, Hook ]
    ).
