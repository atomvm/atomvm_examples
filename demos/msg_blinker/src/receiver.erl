%
% This file is part of AtomVM.
%
% Copyright (c) 2023 Alex Whitney <alex@dudemanco.de>
% All rights reserved.
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%

-module(receiver).
-export([run/0, handle_message/2]).

run() ->
    GPIO = gpio:open(),
    gpio:set_direction(GPIO, 2, output),
    handle_message(GPIO, off).

handle_message(GPIO, State) ->
    receive
        on ->
            gpio:set_level(GPIO, 2, 1),
            io:format("Turned on GPIO pin 2~n"),
            handle_message(GPIO, on);
        off ->
            gpio:set_level(GPIO, 2, 0),
            io:format("Turned off GPIO pin 2~n"),
            handle_message(GPIO, off)
    after 0 ->
        handle_message(GPIO, State)
    end.
