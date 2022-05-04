#
# This file is part of AtomVM.
#
# Copyright 2021-2022 Davide Bettio <davide@uninstall.it>
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
# SPDX-License-Identifier: Apache-2.0
#

defmodule Main do
  def start() do
    :erlang.display("Starting Elixir LISP Faces Example")

    display = :erlang.open_port({:spawn, "display"}, width: 320, height: 240)

    # SDL display has a builtin in keyboard input support
    # FIXME: Using :system_architecture to detect if we are using SDL is just a workaround
    {:ok, input} =
      case :erlang.system_info(:system_architecture) do
        <<"Linux-", _something::binary>> -> {:ok, display}
        _ -> :face.start_link()
      end

    {:ok, t} = :terminalui.start_link([], display_server: display, input_server: input)

    # we didn't register any default IO handler (group_leader), so we cannot use standard IO funcs
    # let's try different ways to send messages to the terminalui
    :gen_server.call(t, {:write, "AtomVM started.\n"}, 60000)

    :gen_server.call(
      t,
      {:write,
       :erlang.integer_to_list(:erlang.system_info(:process_count)) ++ ' running processes.\n'},
      60000
    )

    send(t, {:io_request, self(), make_ref(), {:put_chars, :unicode, <<"AtomVM LISP Ready.\n">>}})

    # let's register it
    :erlang.group_leader(t, self())

    :arepl.start()

    recv_loop()
  end

  defp recv_loop() do
    receive do
      any -> :erlang.display({:got, any})
    end

    recv_loop()
  end
end
