-module(client).
-export([start/1, put/3, del/2, get/2, stats/1]).
-export([text_client/1, bin_client/1]).
-export([task_to_text/1, parse_text/1, get_length/1, task_to_bin/1, parse_bin/1]).

-define(TextPort, 8888).
-define(BinPort, 8889). 

start(ServerPort) ->
    {ok, ServerSocket} = gen_tcp:connect("localhost", ServerPort, [{active, true}]),
    case ServerPort of
        ?TextPort ->
            ClientPid = spawn(text_client(ServerSocket)),
            {ok, ClientPid};
        ?BinPort ->  
            ClientPid = bin,
            {ok, ClientPid};
        _ ->
            {error, "Unspecified Port"}
    end.

task_to_text(Task) ->
    case lists:head(Task) of
        put ->
            io_lib:format("PUT ~p ~p", [lists:nth(2, Task), lists:nth(3, Task)]);
        del ->
            io_lib:format("DEL ~p", [lists:nth(2, Task)]);
        get ->
            io_lib:format("GET ~p", [lists:nth(2, Task)]);
        stats ->
            io_lib:format("STATS")
    end.

parse_text(Reply) ->
    Chunks = string:tokens(Reply, [$\s]),
    case lists:head(Chunks) of
        "OK" -> 
            case length(Chunks) of
                1 -> ok;
                _ -> {ok, lists:nthtail(1, Chunks)}
            end;
        "EINVAL" -> einval;
        "ENOTFOUND" -> enotfound;
        "EBINARY" -> ebinary;
        "EBIG" -> ebig;
        "EUNK" -> eunk
    end.

text_client(ServerSocket) ->
    receive
        {Task, Pid} ->
            Msg = task_to_text(Task),
            gen_tcp:send(ServerSocket, Msg),
            receive 
                {tcp, ServerSocket, TextReply} -> 
                    Reply = parse_text(TextReply),
                    Pid ! Reply
            end;
        _ ->
            {error, ""}
    end,
    text_client(ServerSocket).

get_length(Length) ->
    if
        Length >= 256 ->
            integer_to_list(get_length(Length div 256))++integer_to_list((Length rem 256));
        true ->
            Length
    end.

task_to_bin(Task) ->
    case lists:head(Task) of
        put ->
            BinTask = integer_to_list(11),
            LengthKey = get_length(length(lists:nth(2, Task))),
            LengthValue = get_length(length(lists:nth(3, Task))),
            list_to_binary(BinTask++LengthKey++(lists:nth(2, Task))++LengthValue++(lists:nth(3, Task)));
        del ->
            BinTask = integer_to_list(12),
            LengthKey = get_length(length(lists:nth(2, Task))),
            list_to_binary(BinTask++LengthKey++(lists:nth(2, Task)));
        get ->
            BinTask = integer_to_list(13),
            LengthKey = get_length(length(lists:nth(2, Task))),
            list_to_binary(BinTask++LengthKey++(lists:nth(2, Task)));
        stats ->
            BinTask = integer_to_list(21),
            list_to_binary(BinTask)
    end.

separate([]) -> [];
separate(List) ->
    Length = list_to_integer(lists:nth(1, List)) * math:pow(256, 3) +
             list_to_integer(lists:nth(2, List)) * math:pow(256, 2) +
             list_to_integer(lists:nth(3, List)) * math:pow(256, 1) + 
             list_to_integer(lists:nth(4, List)) * math:pow(256, 0),
    String = lists:sublist(5, Length),
    String++(lists:nthtail(Length+1, List)).

parse_bin(Reply) ->
    Chunks = binary_to_list(Reply),
    case lists:head(Chunks) of
        "101" -> 
            case length(Chunks) of
                1 -> ok;
                _ -> 
                    {ok, separate(lists:nthtail(1, Chunks))}
            end;
        "111" -> einval;
        "112" -> enotfound;
        "113" -> ebinary;
        "114" -> ebig;
        "115" -> eunk
    end.


bin_client(ServerSocket) ->
    receive
        {Task, Pid} ->
            Msg = task_to_bin(Task),
            gen_tcp:send(ServerSocket, Msg),
            receive 
                {tcp, ServerSocket, BinReply} -> 
                    Reply = parse_bin(BinReply),
                    Pid ! Reply
            end;
        _ ->
            {error, ""}
    end,
    text_client(ServerSocket).

put(ClientPid, Key, Value) ->
    ClientPid ! [put, Key, Value],
    receive
        ok -> ok;
        _ -> error
    end.

get(ClientPid, Key) ->
    ClientPid ! [get, Key],
    receive
        {ok, Value} -> {ok, Value};
        _ -> error
    end.

del(ClientPid, Key) ->
    ClientPid ! [del, Key],
    receive
        {ok, Value} -> {ok, Value};
        _ -> error
    end.

stats(ClientPid) ->
    ClientPid ! [stats],
    receive
        {ok, List} -> {ok, List};
        _ -> error
    end.

