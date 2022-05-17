-module(http_client).
-export([client/1]).

client(Path) ->
    Data =
        <<"GET ", Path/binary,
            " HTTP/1.1\r\n"
            "Host: api.openweathermap.org\r\n"
            "\r\n">>,
    Host = "api.openweathermap.org",

    {ok, Sock} = gen_tcp:connect(Host, 80, [binary]),
    ok = gen_tcp:send(Sock, Data),

    {ok, R} = receive_http_response(Sock, 2000),

    ok = gen_tcp:close(Sock),
    {body, Rest} = parse(R),
    jsondes:parse(Rest).

receive_http_response(Sock, Timeout) ->
    receive_http_response(Sock, Timeout, <<"">>).

receive_http_response(Sock, Timeout, Acc) ->
    receive
        {tcp, Sock, Data} ->
            receive_http_response(Sock, Timeout, <<Acc/binary, Data/binary>>)
    after Timeout ->
        case Acc of
            <<"">> -> {error, timeout};
            Rcvd -> {ok, Rcvd}
        end
    end.

parse(Data) ->
    parse_http_status(Data, parsing_http_version).

parse_http_status(<<"HTTP/1.1 ", Rest/binary>>, parsing_http_version) ->
    parse_http_status(Rest, parsing_status_code);
parse_http_status(<<Code:3/binary, Rest/binary>>, parsing_status_code) ->
    parse_http_status(Rest, {Code, parsing_status_message});
parse_http_status(<<"\r\n", Rest/binary>>, {Code, parsing_status_message}) ->
    parse_headers(Rest);
parse_http_status(<<_C, Rest/binary>>, {Code, parsing_status_message}) ->
    parse_http_status(Rest, {Code, parsing_status_message}).

parse_headers(<<"\r\n\r\n", Rest/binary>>) ->
    {body, Rest};
parse_headers(<<_C, Rest/binary>>) ->
    parse_headers(Rest).
