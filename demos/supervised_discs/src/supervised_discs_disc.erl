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

-module(supervised_discs_disc).

-export([start_link/1]).

-behavior(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

-define(TIMEOUT, 10).

start_link(Index) ->
    gen_server:start_link(?MODULE, Index, []).

-record(state, {
    index :: integer(),
    last_time :: integer(),
    pos_x :: integer(),
    pos_y :: integer(),
    speed_x :: integer(),
    speed_y :: integer(),
    handler :: emscripten:listen_handler()
}).

init(Index) ->
    io:format("Starting supervised disc #~B\n", [Index]),
    RandomX = atomvm:random() rem 96,
    RandomY = atomvm:random() rem 96,
    SpeedX = (atomvm:random() rem 4) - 2,
    SpeedY = (atomvm:random() rem 4) - 2,
    RandomH = atomvm:random() rem 360,
    PosX = 2.0 + RandomX,
    PosY = 2.0 + RandomY,

    emscripten:run_script(
        [
            <<"const svg = document.querySelector('#display');">>,
            <<"const circle = document.createElementNS('http://www.w3.org/2000/svg', 'circle');">>,
            <<"circle.setAttributeNS(null, 'id', 'disc-">>,
            integer_to_list(Index),
            <<"');">>,
            <<"circle.setAttributeNS(null, 'cx', '">>,
            float_to_list(PosX),
            <<"%');">>,
            <<"circle.setAttributeNS(null, 'cy', '">>,
            float_to_list(PosY),
            <<"%');">>,
            <<"circle.setAttributeNS(null, 'r', '2%');">>,
            <<"circle.setAttributeNS(null, 'fill', 'hsl(">>,
            integer_to_list(RandomH),
            <<", 100%, 50%)');">>,
            <<"svg.appendChild(circle);">>
        ],
        [main_thread, async]
    ),
    {ok, ListenHandler} = emscripten:register_click_callback("#disc-" ++ integer_to_list(Index)),
    Now = erlang:system_time(millisecond),
    {ok,
        #state{
            index = Index,
            pos_x = PosX,
            pos_y = PosY,
            speed_x = SpeedX,
            speed_y = SpeedY,
            handler = ListenHandler,
            last_time = Now
        },
        ?TIMEOUT}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unimplemented}, State}.

handle_info(timeout, State) ->
    Now = erlang:system_time(millisecond),
    Delta = Now - State#state.last_time,
    Factor = Delta / 100.0,
    NewPosX0 = State#state.pos_x + (State#state.speed_x * Factor),
    NewPosY0 = State#state.pos_y + (State#state.speed_y * Factor),
    {NewPosX1, NewSpeedX1} =
        if
            NewPosX0 > 98 ->
                {2 * 98 - NewPosX0, -State#state.speed_x};
            NewPosX0 < 2 ->
                {2 * 2 - NewPosX0, -State#state.speed_x};
            true ->
                {NewPosX0, State#state.speed_x}
        end,
    {NewPosY1, NewSpeedY1} =
        if
            NewPosY0 > 98 ->
                {2 * 98 - NewPosY0, -State#state.speed_y};
            NewPosY0 < 2 ->
                {2 * 2 - NewPosY0, -State#state.speed_y};
            true ->
                {NewPosY0, State#state.speed_y}
        end,
    emscripten:run_script(
        [
            <<"const disc = document.querySelector('#disc-">>,
            integer_to_list(State#state.index),
            <<"');">>,
            <<"disc.setAttributeNS(null, 'cx', '">>,
            float_to_list(NewPosX1),
            <<"%');">>,
            <<"disc.setAttributeNS(null, 'cy', '">>,
            float_to_list(NewPosY1),
            <<"%');">>
        ],
        [main_thread, async]
    ),
    NewState = State#state{
        pos_x = NewPosX1,
        pos_y = NewPosY1,
        speed_x = NewSpeedX1,
        speed_y = NewSpeedY1,
        last_time = Now
    },
    {noreply, NewState, ?TIMEOUT};
handle_info({emscripten, {click, _}}, State) ->
    gen_server:cast(stats, click),
    {stop, normal, State}.

terminate(_Reason, #state{index = Index, handler = ListenHandler}) ->
    emscripten:unregister_click_callback(ListenHandler),
    emscripten:run_script(
        [
            <<"const disc = document.querySelector('#disc-">>,
            integer_to_list(Index),
            <<"');">>,
            <<"disc.remove()">>
        ],
        [main_thread, async]
    ),
    ok.
