#!/bin/sh
#
# This file is part of AtomVM.
#
# Copyright 2023 Fred Dushin <fred@dushin.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

REBAR="rebar3"
cd erlang
ERLANG_EXAMPLES="$(/bin/ls | grep -v README.md)"
for i in ${ERLANG_EXAMPLES}; do
	cd $i
	rm -rf _build
	${REBAR} atomvm packbeam -l || exit 1
	cd ..
done
cd ..
cd elixir
ELIXIR_EXAMPLES="$(/bin/ls | grep -v README.md)"
for i in ${ELIXIR_EXAMPLES}; do
	cd $i
	rm -rf _build
	mix deps.get && mix atomvm.packbeam || exit 1
	cd ..
done
cd ..
cd demos
DEMO_EXAMPLES="$(/bin/ls | grep -v README.md)"
for i in ${DEMO_EXAMPLES}; do
	cd $i
	rm -rf _build
	if [ -f mix.exs ]; then
		mix deps.get && mix atomvm.packbeam || exit 1
	else
		${REBAR} atomvm packbeam -l || exit 1
	fi
	cd ..
done
