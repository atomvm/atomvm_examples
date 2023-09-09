%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(supervised_discs_stats).

-export([start_link/0]).

-behavior(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

-define(PERIOD, 5000).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

-record(state, {
    sum_latencies :: integer(),
    count_latencies :: integer(),
    score :: integer(),
    last_time :: integer()
}).

init([]) ->
    io:format("Starting stats\n"),
    register(stats, self()),
    emscripten:run_script(
        [
            <<"const svg = document.querySelector('#display');">>,
            <<"const stats = document.createElementNS('http://www.w3.org/2000/svg', 'text');">>,
            <<"stats.setAttributeNS(null, 'id', 'stats');">>,
            <<"stats.setAttributeNS(null, 'x', '95%');">>,
            <<"stats.setAttributeNS(null, 'y', '5%');">>,
            <<"stats.setAttributeNS(null, 'dominant-baseline', 'middle');">>,
            <<"stats.setAttributeNS(null, 'text-anchor', 'end');">>,
            <<"svg.appendChild(stats);">>,
            <<"stats.append('Score: 0');">>
        ],
        [main_thread, async]
    ),
    {ok, #state{score = 0}}.

handle_cast(click, #state{score = Score} = State0) ->
    NewScore = Score + 1,
    State1 = State0#state{score = NewScore},
    update_display(NewScore),
    {noreply, State1}.

handle_call(_Request, _From, State) ->
    {reply, {error, unimplemented}, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{}) ->
    ok.

update_display(Score) ->
    emscripten:run_script(
        [
            <<"const stats = document.querySelector('#stats');">>,
            <<"stats.firstChild.remove();">>,
            <<"stats.append('Score: ">>,
            integer_to_list(Score),
            <<"');">>
        ],
        [main_thread, async]
    ).
