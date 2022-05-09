%
% This file is part of AtomVM.
%
% Copyright 2021-2022 Davide Bettio <davide@uninstall.it>
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
% SPDX-License-Identifier: Apache-2.0
%

-module(face).

-behavior(gen_server).

-export([
    start_link/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {listener, i2c}).

-define(GPIO_NUM, 5).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({subscribe_input}, {Pid, Ref}, _State) ->
    GPIO = gpio:open(),
    gpio:set_direction(GPIO, 5, input),
    gpio:set_int(GPIO, 5, falling),
    I2C = i2c:open([{scl_io_num, 22}, {sda_io_num, 21}, {i2c_clock_hz, 100000}]),
    {reply, ok, #state{listener = Pid, i2c = I2C}};
handle_call(_msg, _from, state) ->
    {reply, error, state}.

handle_cast(_Msg, State) ->
    {reply, error, State}.

handle_info({gpio_interrupt, ?GPIO_NUM}, #state{listener = Listener, i2c = I2C} = State) ->
    Bin = i2c:read_bytes(I2C, 16#08, 1),
    erlang:display(Bin),
    C =
        case Bin of
            <<"\r">> -> $\n;
            <<ACode/integer>> -> ACode
        end,
    erlang:display(
        {Listener, {input_event, self(), erlang:system_time(millisecond), {keyboard, down, C}}}
    ),
    Listener ! {input_event, self(), erlang:system_time(millisecond), {keyboard, down, C}},
    Listener ! {input_event, self(), erlang:system_time(millisecond), {keyboard, up, C}},
    {noreply, State}.
