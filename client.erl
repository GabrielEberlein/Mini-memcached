-module(client).
-export([start/1, loop/1]).
-export([put/3, del/2, get/2, stats/1]).

start(ServerSocket) ->
    {ok, Socket} = gen_tcp:connect("localhost", ServerSocket, [{active, true}]),
    spawn(loop(Socket)).    

loop(Socket) ->
    receive
        {put, K, V, Pid} ->
            Message = io_lib:format("PUT ~p ~p", [K, V]);
        {del, K, Pid} ->
            Message = io_lib:format("DEL ~p", [K]);
        {get, K, Pid} ->
            Message = io_lib:format("GET ~p", [K]);
        {stats, Pid} ->
            Message = io_lib:format("STATS")
    end,
    gen_tcp:send(Socket, Message),
    receive 
        {tcp, Socket, Msg} ->
            Chunks = string:tokens(Msg, [$\s]),
            case lists:head(Chunks) of
                "OK" -> 
                    case length(Chunks) of
                        1 -> PID ! ok;
                        _ -> PID ! {ok, lists:nthtail(1, Chunks)}
                    end;
                "ENOTFOUND" -> 
                    PID ! enotfound;
                "EINVAL" ->
                    PID ! einval
            end
        end
    loop(Socket).

put(ClientPid, K, V) ->
    ClientPid ! {put, K, V, self()},
    receive
        ok ->
            ok;
        enotfound ->
            enotfound;
        einval ->
            einval;
        _ ->
            error   
    end

del(ClientPid, K) ->
    ClientPid ! {del, K, self()},
    receive
        ok ->
            ok;
        enotfound ->
            enotfound;
        einval ->
            einval;
        _ ->
            error   
    end

get(ClientPid, K) ->
    ClientPid ! {get, K, self()},
    receive
        {ok, K} ->
            {ok, K};
        enotfound ->
            enotfound;
        einval ->
            einval;
        _ ->
            error   
    end

stats(ClientPid) ->
    ClientPid ! {stats, self()},
    receive
        {ok, } ->
            ok;
        enotfound ->
            enotfound;
        einval ->
            einval;
        _ ->
            error   
    end