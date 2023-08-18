-module(client).
-export([start/1, put/3, get/2, del/2, stats/1]).
-export([parse/1, handle_receive/2]).

-define(PUT, <<11>>).
-define(DEL, <<12>>).
-define(GET, <<13>>).
-define(STATS, <<21>>).

-define(OK, <<101>>).
-define(EINVAL, <<111>>).
-define(ENOTFOUND, <<112>>).
-define(EBINARY, <<113>>).
-define(EBIG,<<114>>).
-define(EUNK,<<115>>).

start(ServerPort) -> gen_tcp:connect("localhost", ServerPort, [{active, true}]).

parse(X) ->
    Bin = term_to_binary(X),
    BinLength = byte_size(Bin),
    <<BinLength:32, Bin/binary>>.   

parse_response(Socket, Task) ->
    case Task of
        put -> ok;
        del -> ok;
        _ ->
            case gen_tcp:recv(Socket, 4) of
                {ok, Data} -> 
                    <<Length:32/integer-big>> = Data,
                    case gen_tcp:recv(Socket, Length) of
                        {ok, Data} -> 
                            case Task of
                                get -> {ok, binary_to_term(Data)};
                                stats -> {ok, binary_to_list(Data)}
                            end;
                        {error, Type} -> Type
                    end;
                {error, Type} -> Type
            end
    end.

handle_receive(Socket, Task) ->
    case gen_tcp:recv(Socket, 1) of
        {ok, Data} -> 
            case Data of
                ?OK -> parse_response(Socket, Task);
                ?EINVAL -> einval;
                ?ENOTFOUND -> enotfound;
                ?EBINARY -> ebinary;
                ?EBIG -> ebig;
                ?EUNK -> eunk
            end;
        {error, Type} -> Type
    end.

put(Client, Key, Value) ->
    ParsedKey = parse(Key),
    ParsedValue = parse(Value),
    gen_tcp:send(Client, <<?PUT/binary, ParsedKey/binary, ParsedValue/binary>>),
    handle_receive(Client, put).

get(Client, Key) ->
    ParsedKey = parse(Key),
    gen_tcp:send(Client, <<?GET/binary, ParsedKey/binary>>),
    handle_receive(Client, get).

del(Client, Key) ->
    ParsedKey = parse(Key),
    gen_tcp:send(Client, <<?DEL/binary, ParsedKey/binary>>),
    handle_receive(Client, del).

stats(Client) ->
    gen_tcp:send(Client, ?STATS),
    handle_receive(Client, stats).