#
# This file is part of AtomVM.
#
# Copyright 2026 Masatoshi Nishiguchi
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

defmodule EspRtcMemory do
  @moduledoc """
  Demonstrates ESP32 RTC slow memory usage by persisting a restart counter.
  """

  @compile {:no_warn_undefined, :atomvm}
  @compile {:no_warn_undefined, :esp}

  @restart_delay_ms 10_000

  def start do
    case verify_platform(:atomvm.platform()) do
      :ok ->
        load_state()
        |> increment_state()
        |> save_state()

        IO.puts("Restarting in 10 seconds ...")
        Process.sleep(@restart_delay_ms)
        :esp.restart()

      error ->
        error
    end
  end

  defp load_state do
    IO.puts("Loading count from RTC slow memory ...")

    encoded_term =
      try do
        :esp.rtc_slow_get_binary()
      catch
        :error, :badarg ->
          IO.puts("This device has not recorded the number of starts. Initializing to 0.")
          :erlang.term_to_binary(%RebootState{})
      end

    case safe_binary_to_term(encoded_term) do
      {:ok, %RebootState{count: count} = state} when is_integer(count) and count >= 0 ->
        state

      _ ->
        IO.puts("Error: bad value, resetting to 0.")
        %RebootState{}
    end
  end

  defp safe_binary_to_term(encoded_term) when is_binary(encoded_term) do
    try do
      {:ok, :erlang.binary_to_term(encoded_term)}
    catch
      :error, _ ->
        :error
    end
  end

  defp save_state(current_state) do
    IO.puts("Saving count to RTC slow memory ...")

    current_state
    |> :erlang.term_to_binary()
    |> :esp.rtc_slow_set_binary()
  end

  defp increment_state(%RebootState{count: count} = state) when is_integer(count) and count >= 0 do
    new_count = count + 1
    IO.puts("This device has restarted #{new_count} time(s).")
    %RebootState{state | count: new_count}
  end

  defp increment_state(_) do
    IO.puts("Error: bad value, resetting to 0.")
    %RebootState{}
  end

  defp verify_platform(:esp32), do: :ok
  defp verify_platform(platform), do: {:error, {:unsupported_platform, platform}}
end

