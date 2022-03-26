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

-module(system_info).

-export([start/0]).

start() ->
    SystemInfo = #{
        atom_count => erlang:system_info(atom_count),
        process_count => erlang:system_info(process_count),
        port_count => erlang:system_info(port_count),
        word_size => erlang:system_info(wordsize),
        system_architecture => erlang:system_info(system_architecture)
    },
    io:format("SystemInfo: ~p~n", [SystemInfo]),

    PlatformInfo =
        case atomvm:platform() of
            esp32 ->
                #{
                    esp32_free_heap_size => erlang:system_info(esp32_free_heap_size),
                    esp32_largest_free_block => erlang:system_info(esp32_largest_free_block),
                    esp32_minimum_free_size => erlang:system_info(esp32_minimum_free_size),
                    esp32_chip_info => erlang:system_info(esp32_chip_info),
                    esp_idf_version => erlang:system_info(esp_idf_version)
                };
            _ ->
                #{}
        end,
    io:format("PlatformInfo: ~p~n", [PlatformInfo]),

    Processes = erlang:processes(),
    ProcessInfo = [{Process, get_process_info(Process)} || Process <- Processes],
    io:format("ProcessInfo: ~p~n", [ProcessInfo]),

    ok.

get_process_info(Pid) ->
    [
        erlang:process_info(Pid, heap_size),
        erlang:process_info(Pid, stack_size),
        erlang:process_info(Pid, message_queue_len),
        erlang:process_info(Pid, memory)
    ].
