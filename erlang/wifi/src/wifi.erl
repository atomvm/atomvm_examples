%
% This file is part of AtomVM.
%
% Copyright 2020 Fred Dushin <fred@dushin.net>
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

-module(wifi).

-export([start/0]).

start() ->
    case verify_platform(atomvm:platform()) of
        ok ->
            start_network();
        Error ->
            Error
    end.

start_network() ->
    Config = [
        {ap, [
            {ap_started, fun ap_started/0},
            {sta_connected, fun sta_connected/1},
            {sta_ip_assigned, fun sta_ip_assigned/1},
            {sta_disconnected, fun sta_disconnected/1}
            | maps:get(ap, config:get())
        ]},
        {sta, [
            {connected, fun connected/0},
            {got_ip, fun got_ip/1},
            {disconnected, fun disconnected/0}
            | maps:get(sta, config:get())
        ]},
        {sntp, [{host, "pool.ntp.org"}]}
    ],
    case network:start(Config) of
        ok ->
            timer:sleep(infinity);
        Error ->
            Error
    end.

ap_started() ->
    io:format("AP started.~n").

sta_connected(Mac) ->
    io:format("STA connected with mac ~p~n", [Mac]).

sta_disconnected(Mac) ->
    io:format("STA disconnected with mac ~p~n", [Mac]).

sta_ip_assigned(Address) ->
    io:format("STA assigned address ~p~n", [Address]).

connected() ->
    io:format("STA connected.~n").

got_ip(IpInfo) ->
    io:format("Got IP: ~p.~n", [IpInfo]),
    loop().

disconnected() ->
    io:format("STA disconnected.~n").

verify_platform(esp32) ->
    ok;
verify_platform(Platform) ->
    {error, {unsupported_platform, Platform}}.

loop() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
    io:format("Date: ~p/~p/~p ~p:~p:~p (~pms)~n", [
        Year, Month, Day, Hour, Minute, Second, erlang:system_time(millisecond)
    ]),
    timer:sleep(5000),
    loop().
