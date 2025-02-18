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

-module(sender).
-export([send_messages/0]).

send_messages() ->
    ReceiverPid = whereis(receiver),
    ReceiverPid ! on,
    timer:sleep(2000),
    ReceiverPid ! off,
    timer:sleep(2000),
    send_messages().
