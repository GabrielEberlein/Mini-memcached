-module(client).
-export([start/1, put/3, get/2, del/2, stats/1]).
-export([parse_to_binary/1, handle_receive/2, parse_response/2]).

% Macros para las distintas instrucciones
-define(PUT, <<11>>).
-define(DEL, <<12>>).
-define(GET, <<13>>).
-define(STATS, <<21>>).

% Macros para las distintas respuestas
-define(OK, <<101>>).
-define(EINVAL, <<111>>).
-define(ENOTFOUND, <<112>>).
-define(EBINARY, <<113>>).
-define(EBIG, <<114>>).
-define(EUNK, <<115>>).
-define(ENOMEMORY, <<116>>).

% Conecta con el servidor especificado, devuelve el identificador
% de Socket abierto para tal conección.
start(ServerPort) -> gen_tcp:connect("localhost", ServerPort, [{active, false}, {mode, binary}]).

% Parsea el argumento a la especificación en binario dada por el enunciado
parse_to_binary(X) ->
    Binary = term_to_binary(X),
    BinLength = byte_size(Binary),
    <<BinLength:32, Binary/binary>>.

% Parsea los argumentos de la respuesta segun la especificación en binario dada por el enunciado
% Comprueba si ocurre un error de conección al intentar recibir los mismos
parse_response(Socket, Task) ->
    case Task of
        put -> ok;
        del -> ok;
        _ ->
            case gen_tcp:recv(Socket, 4) of
                {ok, Data} -> 
                    <<Length:32/integer-big>> = Data,
                    case gen_tcp:recv(Socket, Length) of
                        {ok, Data2} -> 
                            case Task of
                                get -> {ok, binary_to_term(Data2)};
                                stats -> {ok, binary_to_list(Data2)}
                            end;
                        {error, Type2} -> Type2
                    end;
                {error, Type} -> Type
            end
    end.

% Maneja el resultado devuelto por el servidor
% En el caso de haber devuelto un OK, parsea los argumentos retornados
% En el caso de haber devuelto un Error, especifica el mismo
% Comprueba si ocurre un error de conección al intentar recibir los mismos
handle_receive(Socket, Task) ->
    case gen_tcp:recv(Socket, 1) of
        {ok, Data} ->
            case Data of
                ?OK -> parse_response(Socket, Task);
                ?EINVAL -> einval;
                ?ENOTFOUND -> enotfound;
                ?EBINARY -> ebinary;
                ?EBIG -> ebig;
                ?EUNK -> eunk;
                ?ENOMEMORY -> enomemory
            end;
        {error, Type} -> Type
    end.

% Ejecuta la instrucción Put al socket especificado con los argumentos dados
% y obtiene el resultado de la misma
put(Socket, Key, Value) ->
    ParsedKey = parse_to_binary(Key),
    ParsedValue = parse_to_binary(Value),
    gen_tcp:send(Socket, <<?PUT/binary, ParsedKey/binary, ParsedValue/binary>>),
    handle_receive(Socket, put).

% Ejecuta la instrucción Get al socket especificado con los argumentos dados
% y obtiene el resultado de la misma
get(Socket, Key) ->
    ParsedKey = parse_to_binary(Key),
    gen_tcp:send(Socket, <<?GET/binary, ParsedKey/binary>>),
    handle_receive(Socket, get).

% Ejecuta la instrucción Del al socket especificado con los argumentos dados
% y obtiene el resultado de la misma
del(Socket, Key) ->
    ParsedKey = parse_to_binary(Key),
    gen_tcp:send(Socket, <<?DEL/binary, ParsedKey/binary>>),
    handle_receive(Socket, del).

% Ejecuta la instrucción Stats al socket especificado y obtiene el resultado de la misma
stats(Socket) ->
    gen_tcp:send(Socket, ?STATS),
    handle_receive(Socket, stats).
