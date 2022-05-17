-module(wifi_conn).

-export([connect/2]).

connect(Ssid, Psk) ->
    try try_connect(Ssid, Psk) of
        Any -> Any
    catch
        _T:_E -> not_connected
    end.

try_connect(Ssid, Psk) ->
    Creds = [
        {ssid, Ssid},
        {psk, Psk}
    ],
    case network:wait_for_sta(Creds, 30000) of
        {ok, {Address, Netmask, Gateway}} ->
            io:format(
                "Acquired IP address: ~p Netmask: ~p Gateway: ~p~n",
                [to_string(Address), to_string(Netmask), to_string(Gateway)]
            ),
            connected;
        Error ->
            io:format("An error occurred starting network: ~p~n", [Error]),
            not_connected
    end.

to_string({{A, B, C, D}, Port}) ->
    io_lib:format("~p.~p.~p.~p:~p", [A, B, C, D, Port]);
to_string({A, B, C, D}) ->
    io_lib:format("~p.~p.~p.~p", [A, B, C, D]).
