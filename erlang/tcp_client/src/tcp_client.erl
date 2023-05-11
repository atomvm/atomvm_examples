%
% This file is part of AtomVM.
%
% Copyright 2019-2020 Fred Dushin <fred@dushin.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(tcp_client).

-export([start/0]).

start() ->
    ok = maybe_start_network(atomvm:platform()),
    Host = maps:get(host, config:get()),
    Port = maps:get(port, config:get()),
    case gen_tcp:connect(Host, Port, []) of
        {ok, Socket} ->
            io:format("Connected to ~p from ~p~n", [peer_address(Socket), local_address(Socket)]),
            loop(Socket);
        Error ->
            io:format("An error occurred connecting: ~p~n", [Error])
    end.

loop(Socket) ->
    SendPacket = <<"AtomVM rocks!">>,
    case gen_tcp:send(Socket, SendPacket) of
        ok ->
            io:format("Sent ~p to ~p\n", [SendPacket, peer_address(Socket)]),
            receive
                {tcp_closed, Socket} ->
                    io:format("Connection closed.~n"),
                    ok;
                {tcp_error, Socket, Error} ->
                    io:format("TCP error ~p.~n", [Error]),
                    ok;
                {tcp, Socket, ReceivedPacket} ->
                    io:format("Received ~p from ~p~n", [ReceivedPacket, peer_address(Socket)]),
                    timer:sleep(1000),
                    loop(Socket);
                Other ->
                    io:format("Unexpected message ~p~n", [Other]),
                    loop(Socket)
            end;
        Error ->
            io:format("An error occurred sending a packet: ~p~n", [Error])
    end.

local_address(Socket) ->
    {ok, SockName} = inet:sockname(Socket),
    to_string(SockName).

peer_address(Socket) ->
    {ok, Peername} = inet:peername(Socket),
    to_string(Peername).

to_string({A, B, C, D}) ->
    io_lib:format("~p.~p.~p.~p", [A, B, C, D]);
to_string({Address, Port}) ->
    io_lib:format("~s:~p", [to_string(Address), Port]).

maybe_start_network(esp32) ->
    Config = maps:get(sta, config:get()),
    case network:wait_for_sta(Config, 30000) of
        {ok, {Address, Netmask, Gateway}} ->
            io:format(
                "Acquired IP address: ~p Netmask: ~p Gateway: ~p~n",
                [to_string(Address), to_string(Netmask), to_string(Gateway)]
            ),
            ok;
        Error ->
            io:format("An error occurred starting network: ~p~n", [Error]),
            Error
    end;
maybe_start_network(_Platform) ->
    ok.
