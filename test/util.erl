-module(util).
-export([test_good/1, test_errors/1]).

test_good(Cases) ->
    lists:foreach(fun(Case) -> check_good(Case) end, Cases).

test_errors(Cases) ->
    lists:foreach(fun(Case) -> check_error(Case) end, Cases).

ok_dec(J, E) ->
    lists:flatten(io_lib:format("Decoding ~p gives ~p", [J, E])).

ok_enc(E, J) ->
    lists:flatten(io_lib:format("Encoding ~p gives ~p", [E, J])).

error_mesg(J) ->
    lists:flatten(io_lib:format("Decoding ~p returns an error.", [J])).

check_good({J, E}) ->
    etap:is(json:decode(J), {ok, E}, ok_dec(J, E)),
    case json:encode(E) of
    {ok, IoList} ->
        etap:is(iolist_to_binary(IoList), J, ok_enc(E, J));
    Error ->
        etap:is(Error, {ok, J}, ok_enc(E, J))
    end;
check_good({J, E, J2}) ->
    etap:is(json:decode(J), {ok, E}, ok_dec(J, E)),
    case json:encode(E) of
    {ok, IoList} ->
        etap:is(iolist_to_binary(IoList), J2, ok_enc(E, J2));
    Error ->
        etap:is(Error, {ok, J2}, ok_enc(E, J2))
    end.

check_error(J) ->
    etap:fun_is(
        fun({error, _}) -> true; (_) -> false end,
        json:decode(J),
        error_mesg(J)
    ).
