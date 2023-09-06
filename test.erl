-module(test).
-import(client, [start/1, put/3, get/2, del/2, stats/1]).
-export([spawn_n/1, test_client/2]).

spawn_n(0) -> 
    ok;
spawn_n(N) -> 
    {ok, S} = start(889),
    put(S, "B", "123"),
    spawn(?MODULE, test_client, [N, S]),
    spawn_n(N-1).

test_client(N, S) ->
    %KEY = string:copies("A", N),
    put(S, "AA", "123"),
    get(S, "A"),
    del(S, "A"),
    stats(S),
    test_client(N, S).