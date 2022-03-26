%
% This file is part of AtomVM.
%
% Copyright 2022 Fred Dushin <fred@dushin.net>
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

-module(gpio_interrupt).

-export([start/0]).

-define(PIN, 2).

start() ->
    case verify_platform(atomvm:platform()) of
        ok ->
            gpio:set_pin_mode(?PIN, input),
            gpio:set_pin_pull(?PIN, down),
            GPIO = gpio:start(),
            gpio:set_int(GPIO, ?PIN, rising),
            loop();
        Error ->
            Error
    end.

loop() ->
    io:format("Waiting for interrupt ... "),
    receive
        {gpio_interrupt, Pin} ->
            io:format("Interrupt on pin ~p~n", [Pin])
    end,
    loop().

verify_platform(esp32) ->
    ok;
verify_platform(stm32) ->
    ok;
verify_platform(Platform) ->
    {error, {unsupported_platform, Platform}}.
