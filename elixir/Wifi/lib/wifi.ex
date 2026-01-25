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

defmodule Wifi do
  @moduledoc """
  Wi-Fi example for Elixir.
  """

  @compile {:no_warn_undefined, :atomvm}
  @compile {:no_warn_undefined, :network}

  @sta_config Application.compile_env!(:wifi, :sta)
  @ap_config Application.compile_env!(:wifi, :ap)

  @clock_interval_ms 5_000

  def start do
    case verify_platform(:atomvm.platform()) do
      :ok ->
        start_network()

      error ->
        error
    end
  end

  defp start_network() do
    network_config = [
      ap:
        [
          ap_started: &ap_started/0,
          sta_connected: &sta_connected/1,
          sta_ip_assigned: &sta_ip_assigned/1,
          sta_disconnected: &sta_disconnected/1
        ] ++ @ap_config,
      sta:
        [
          connected: &connected/0,
          got_ip: &got_ip/1,
          disconnected: &disconnected/0
        ] ++ @sta_config,
      sntp: [
        host: "time-d-b.nist.gov",
        synchronized: &sntp_synchronized/1
      ]
    ]

    case :network.start(network_config) do
      {:ok, _pid} ->
        IO.puts("wifi: network started")
        Process.sleep(:infinity)

      error ->
        error
    end
  end

  defp ap_started, do: IO.puts("AP started")

  defp sta_connected(mac), do: IO.puts("STA connected with mac #{inspect(mac)}")

  defp sta_disconnected(mac), do: IO.puts("STA disconnected with mac #{inspect(mac)}")

  defp sta_ip_assigned(address), do: IO.puts("STA assigned address #{inspect(address)}")

  defp connected, do: IO.puts("STA connected")

  defp disconnected, do: IO.puts("STA disconnected")

  defp got_ip(ip_info) do
    IO.puts("Got IP: #{inspect(ip_info)}")
    _clock_pid = spawn(fn -> clock_loop() end)
    :ok
  end

  defp sntp_synchronized(timeval = {_tv_sec, _tv_usec}) do
    IO.puts("Synchronized time with SNTP server #{inspect(timeval)}")
  end

  defp verify_platform(:esp32), do: :ok
  defp verify_platform(:pico), do: :ok
  defp verify_platform(platform), do: {:error, {:unsupported_platform, platform}}

  defp clock_loop do
    {{year, month, day}, {hour, minute, second}} = :erlang.universaltime()
    ms = :erlang.system_time(:millisecond)

    :io.format(
      ~c"Date: ~4..0B/~2..0B/~2..0B ~2..0B:~2..0B:~2..0B (~pms)~n",
      [year, month, day, hour, minute, second, ms]
    )

    Process.sleep(@clock_interval_ms)
    clock_loop()
  end
end
