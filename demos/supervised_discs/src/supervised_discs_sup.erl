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

-module(supervised_discs_sup).

-export([start/1, init/1]).

start(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(_Args) ->
    Cores = erlang:system_info(schedulers_online),
    Random = atomvm:random() rem Cores,
    NumCircles = Cores + Random,
    io:format("We are randomly creating ~B supervised discs\n", [NumCircles]),
    DiscChildrenSpecs = [
        {
            supervised_discs_disc,
            {supervised_discs_disc, start_link, [Index]},
            permanent,
            brutal_kill,
            worker,
            [supervised_discs_disc]
        }
     || Index <- lists:seq(1, NumCircles)
    ],
    ChildSpecs =
        [
            {
                supervised_discs_stats,
                {supervised_discs_stats, start_link, []},
                permanent,
                brutal_kill,
                worker,
                [supervised_discs_stats]
            }
        ] ++ DiscChildrenSpecs,
    SupFlags = {one_for_one, 1, 1},
    {ok, {
        SupFlags,
        ChildSpecs
    }}.
