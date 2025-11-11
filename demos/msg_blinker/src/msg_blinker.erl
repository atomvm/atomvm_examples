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

-module(msg_blinker).

-export([start/0]).
start() ->
    Pid1 = spawn(receiver, run, []),
    register(receiver, Pid1),
    Pid2 = spawn(sender, send_messages, []),
    register(sender, Pid2),
    wait_forever().

wait_forever() ->
    receive
        X -> X
    end.
