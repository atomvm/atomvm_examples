%
% This file is part of AtomVM.
%
% Copyright 2018-2020 Davide Bettio <davide@uninstall.it>
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

-module(blinky).
-export([start/0]).

% External LED on pin 2 will work with all pico devices.
-define(PIN, 2).
% Comment out above and uncomment the following line to use pico-w onboard LED.
% -define(PIN, {wl, 0}).

start() ->
    platform_gpio_setup(atomvm:platform()),
    loop(pin(), low).

loop(Pin, Level) ->
    io:format("Setting pin ~p ~p~n", [Pin, Level]),
    gpio:digital_write(Pin, Level),
    timer:sleep(1000),
    loop(Pin, toggle(Level)).

toggle(high) ->
    low;
toggle(low) ->
    high.

pin() ->
    case atomvm:platform() of
        esp32 ->
            2;
        pico ->
            ?PIN;
        stm32 ->
            {b, 0};
        Platform ->
            erlang:exit({unsupported_platform, Platform})
    end.

platform_gpio_setup(esp32) ->
    gpio:set_pin_mode(pin(), output);
platform_gpio_setup(stm32) ->
    gpio:set_pin_mode(pin(), output);
platform_gpio_setup(pico) ->
    case ?PIN of
        {wl, 0} ->
            % Pico-W needs no setup for extra "WL" pins
            ok;
        Pin ->
            % Setup for Pico GPIO pins
            gpio:init(Pin),
            gpio:set_pin_mode(Pin, output)
    end;
platform_gpio_setup(Platform) ->
    io:format("Platform ~p is not supported.~n", [Platform]),
    erlang:exit({error, {unsupported_platform, Platform}}).
