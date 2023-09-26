-module(test).
-import(client, [start/1, put/3, get/2, del/2, stats/1]).
-export([spawn_n/1, test_client/2]).

spawn_n(0) -> 
    ok;
spawn_n(N) -> 
    {ok, S} = start(889),
    spawn(?MODULE, test_client, [N, S]),
    spawn_n(N-1).

test_client(N, S) ->
    % KEY = string:copies("A", N),
    % put(S, KEY, "123"),
    % get(S, KEY),
    % del(S, KEY),
    % stats(S),
    test_client(N, S).