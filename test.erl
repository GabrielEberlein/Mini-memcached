-module(test).
-import(client, [start/1, put/3, get/2, del/2, stats/1]).
-export([spawn_n/1, test_client/1]).

random_String(0, Acc) ->
    lists:reverse(Acc);
random_String(Length, Acc) ->
    RandomChar = random_Char(),
    random_String(Length - 1, [RandomChar | Acc]).

random_Char() ->
    AlphaNumeric = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
    Index = rand:uniform(length(AlphaNumeric)),
    lists:nth(Index, AlphaNumeric).

spawn_n(0) -> 
    ok;
spawn_n(N) -> 
    {ok, S} = start(889),
    spawn(?MODULE, test_client, [S]),
    spawn_n(N-1).

test_client(S) ->
    LenKey = rand:uniform(100000),
    LenVal = rand:uniform(100000),
    Key = random_String(LenKey, []),
    Val = random_String(LenVal, []),
    put(S, Key, Val),
    get(S, Key),
    % del(S, Key),
    % stats(S),
    test_client(S).