-module(json).
-export([encode/1, decode/1, fuzz/0, fuzz/1]).
-on_load(init/0).

init() ->
    SoName = case code:priv_dir(json) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, json]);
                _ ->
                    filename:join([priv, json])
            end;
        Dir ->
            filename:join(Dir, json)
    end,
    erlang:load_nif(SoName, 0).

decode(IoList) ->
    case reverse_tokens(IoList) of
    {ok, ReverseTokens} ->
        [[EJson]] = make_ejson(ReverseTokens, [[]]),
        {ok, EJson};
    Error ->
        Error
    end.

encode(EJson) ->
    try
        {ok, do_encode(EJson)}
    catch exit:{encode, Error} ->
        {error, Error}
    end.


do_encode(true) ->
    <<"true">>;
do_encode(false) ->
    <<"false">>;
do_encode(null) ->
    <<"null">>;
do_encode(I) when is_integer(I) ->
    integer_to_list(I);
do_encode(F) when is_float(F) ->
    encode_double(F);
do_encode(S) when is_binary(S); is_atom(S) ->
    do_encode_string(S);
do_encode({Props}) when is_list(Props) ->
    encode_proplist(Props);
do_encode(Array) when is_list(Array) ->
    encode_array(Array);
do_encode(Bad) ->
    exit({encode, {bad_term, Bad}}).

encode_array([]) ->
    <<"[]">>;
encode_array(L) ->
    F = fun (O, Acc) ->
                [$,, do_encode(O) | Acc]
        end,
    [$, | Acc1] = lists:foldl(F, "[", L),
    lists:reverse([$\] | Acc1]).

encode_proplist([]) ->
    <<"{}">>;
encode_proplist(Props) ->
    F = fun ({K, V}, Acc) ->
                KS = do_encode_string(K),
                VS = do_encode(V),
                [$,, VS, $:, KS | Acc]
        end,
    [$, | Acc1] = lists:foldl(F, "{", Props),
    lists:reverse([$\} | Acc1]).

do_encode_string(S) ->
    case encode_string(S) of
    {error, Error} ->
        exit({encode, {string_encode_failure, Error, S}});
    String ->
        <<$", String/binary, $">>
    end.


encode_string(_) ->
    not_loaded(?LINE).


encode_double(_) ->
    not_loaded(?LINE).


make_ejson([], Stack) ->
    Stack;
make_ejson([0 | RevEvs], [ArrayValues, PrevValues | RestStack]) ->
    % 0 ArrayStart
    make_ejson(RevEvs, [[ArrayValues | PrevValues] | RestStack]);
make_ejson([1 | RevEvs], Stack) ->
    % 1 ArrayEnd
    make_ejson(RevEvs, [[] | Stack]);
make_ejson([2 | RevEvs], [ObjValues, PrevValues | RestStack]) ->
    % 2 ObjectStart
    make_ejson(RevEvs, [[{ObjValues} | PrevValues] | RestStack]);
make_ejson([3 | RevEvs], Stack) ->
    % 3 ObjectEnd
    make_ejson(RevEvs, [[] | Stack]);
make_ejson([{0, Value} | RevEvs], [Vals | RestStack] = _Stack) ->
    % {0 , IntegerString}
    make_ejson(RevEvs, [[to_int(Value) | Vals] | RestStack]);
make_ejson([{1, Value} | RevEvs], [Vals | RestStack] = _Stack) ->
    % {1, FloatString}
    make_ejson(RevEvs, [[to_float(Value) | Vals] | RestStack]);
make_ejson([{3, String} | RevEvs], [[PrevValue|RestObject] | RestStack] = _Stack) ->
    % {3 , ObjectKey}
    make_ejson(RevEvs, [[{String, PrevValue}|RestObject] | RestStack]);
make_ejson([Value | RevEvs], [Vals | RestStack] = _Stack) ->
    make_ejson(RevEvs, [[Value | Vals] | RestStack]).

fuzz() ->
    json_fuzz:fuzz(fun json_fuzz:choose/4).
fuzz(Chooser) ->
    json_fuzz:fuzz(Chooser).

not_loaded(Line) ->
    exit({json_not_loaded, module, ?MODULE, line, Line}).
    
reverse_tokens(_) ->
    not_loaded(?LINE).

to_int(B) when is_binary(B) ->
    to_int(binary_to_list(B));
to_int(L) ->
    list_to_integer(L).

to_float(B) when is_binary(B) ->
    to_float(binary_to_list(B));
to_float(L) ->
    list_to_float(L).
