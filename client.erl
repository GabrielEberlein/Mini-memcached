-module(client).
-export([start/1, text_client/1, bin_client/1]).
-export([put/3, del/2, get/2, stats/1]).

start(ServerSocket) ->
    {ok, Socket} = gen_tcp:connect("localhost", ServerSocket, [{active, true}]),
    case ServerSocket of
        8888 ->
            ClientPid = spawn(text_client(Socket));
        8889 ->  
            ClientPid = spawn(bin_client(Socket))
    end,
    {ok, ClientPid}.

text_client(Socket) ->
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
    text_client(Socket).

get_length(Length) ->
    if
        Length >= 256 ->
            integer_to_list(get_length(Length div 256))++integer_to_list((Length rem 256));
        true ->
            Length
    end.

bin_client(Socket) ->
    receive
        {put, K, V, Pid} ->
            Task = integer_to_list(11),
            LengthKey = get_length(length(K)),
            LengthValue = get_length(length(V)),
            Message = list_to_binary(Task++LengthKey++K++LengthValue++V);
        {del, K, Pid} ->
            Task = integer_to_list(12),
            LengthKey = get_length(length(K)),
            Message = list_to_binary(Task++LengthKey++K);
        {get, K, Pid} ->
            Task = integer_to_list(13),
            LengthKey = get_length(length(K)),
            Message = list_to_binary(Task++LengthKey++K);
        {stats, Pid} ->
            Task = integer_to_list(21),
            Message = list_to_binary(Task)
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
    bin_client(Socket).

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