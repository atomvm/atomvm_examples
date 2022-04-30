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

-module(terminalui).

-export([
    start_link/2,
    append/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    handle_input/4
]).

-define(FONT, default16px).
-define(X_MARGIN, 8).
-define(Y_MARGIN, 8).
-define(LINE_H, 16).

-record(termstate, {
    lines = [[]], cursor_x = 0, cursor_y = 0, mode = default, buffer, pending_pid, pending_ref
}).

start_link(Args, Opts) ->
    avm_scene:start_link(?MODULE, Args, Opts).

init([]) ->
    {ok, #termstate{}}.

handle_call({write, Data}, _from, State) ->
    {NextState, Scene} = do_write(Data, State),
    {reply, ok, NextState, [{push, Scene}]}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({io_request, FPid, FRef, Request} = F, State) ->
    {NextState, Scene} = io_request(Request, FPid, FRef, State),
    {noreply, NextState, [{push, Scene}]};
handle_info(Msg, State) ->
    {noreply, State}.

handle_input({keyboard, down, Char}, _Timestamp, _From, State) ->
    NewModeState = maybe_input_change_mode(Char, State),
    {NextState, Scene} = do_write([Char], NewModeState),
    {noreply, NextState, [{push, Scene}]};
handle_input(_Other, _Timestamp, _From, State) ->
    {noreply, State}.

do_write(Any, State) ->
    NextLines = append(Any, State#termstate.lines),
    RLines = lists:reverse(NextLines),
    LinesScene = lines_to_scene(RLines, 0),
    % TODO: add cursor: %++ [{rect, ?X_MARGIN + State#termstate.cursor_x, ?Y_MARGIN + State#termstate.cursor_y, 8, ?LINE_H, 16#00}],
    Scene = LinesScene,
    {State#termstate{lines = NextLines}, Scene}.

io_request({get_line, unicode, Data}, FPid, FRef, State) ->
    NewModeState = State#termstate{
        mode = get_line, pending_pid = FPid, pending_ref = FRef, buffer = []
    },
    do_write(Data, NewModeState);
io_request({put_chars, unicode, Data}, FPid, FRef, State) ->
    R = do_write(Data, State),
    FPid ! {io_reply, FRef, ok},
    R.

maybe_input_change_mode($\n, #termstate{mode = get_line} = State) ->
    NewBuffer = State#termstate.buffer ++ [$\n],
    Reply = {io_reply, State#termstate.pending_ref, NewBuffer},
    State#termstate.pending_pid ! Reply,
    State#termstate{
        mode = default, pending_pid = undefined, pending_ref = undefined, buffer = undefined
    };
maybe_input_change_mode(Char, #termstate{mode = get_line} = State) ->
    NewBuffer = State#termstate.buffer ++ [Char],
    State#termstate{buffer = NewBuffer};
maybe_input_change_mode(_Char, State) ->
    State.

lines_to_scene([], _Pos) ->
    [];
lines_to_scene([H | T], Pos) ->
    [
        {text, ?X_MARGIN, ?Y_MARGIN + ?LINE_H * Pos, ?FONT, 16#00, 16#FFFFFF, H}
        | lines_to_scene(T, Pos + 1)
    ].

append(Bin, Lines) when is_binary(Bin) ->
    append(erlang:binary_to_list(Bin), Lines);
append([], Lines) ->
    Lines;
append([H | T], Lines) ->
    NewLines = append(H, Lines),
    append(T, NewLines);
append($\n, Lines) ->
    [[] | Lines];
append(Char, Lines) when is_integer(Char) ->
    [LastLine | Tail] = Lines,
    [LastLine ++ [Char] | Tail].
