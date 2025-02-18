#
# This file is part of AtomVM.
#
# Copyright 2025 Tommaso Maioli <tommaso.maioli@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the 'License');
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an 'AS IS' BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

defmodule APIRequest do
  import Secrets

  def start() do
    IO.puts("starting API Request application\n")
    spawn(fn -> start_network() end)

    case wait_for_network() do
      :ok ->
        IO.puts("Connected to WiFi! Making HTTP request...\n")

      {:error, reason} ->
        IO.puts("Failed to connect to WiFi: #{reason}\n")
    end
  end

  defp start_network() do
    IO.puts("Starting network...\n")
    wifi_cfg = get_secret(:wifi_cfg)

    config = [
      sta: [
        connected: fn ->
          IO.puts("WiFi connected\n")
          spawn(fn -> make_request() end)
        end,
        got_ip: fn info -> IO.puts("Got IP: #{info}\n") end,
        disconnected: fn -> IO.puts("WiFi disconnected\n") end,
        ssid: wifi_cfg[:ssid],
        psk: wifi_cfg[:psk]
      ]
    ]

    case verify_platform(:atomvm.platform()) do
      :ok ->
        IO.puts("Starting network...\n")

        case :network.start(config) do
          {:ok, _pid} ->
            IO.puts("Network started.\n")
            # Keep the network process alive
            Process.sleep(:infinity)

          error ->
            error
        end

      error ->
        error
    end
  end

  defp verify_platform(:esp32), do: :ok
  defp verify_platform(platform), do: {:error, {:unsupported_platform, platform}}

  defp wait_for_network() do
    IO.puts("Waiting for network...\n")

    receive do
      {:network, :got_ip, _info} -> :ok
    after
      30_000 -> {:error, :timeout}
    end
  end

  defp make_request() do
    IO.puts("Making HTTPS request to api.sunrisesunset.io API...\n")

    ssl_opts = [{:active, false}, {:verify, :verify_none}]
    ok = :ssl.start()
    IO.puts("TRYING CONN\n")

    case :ahttp_client.connect(:https, ~c"api.sunrisesunset.io", 443, ssl_opts) do
      {:ok, conn} ->
        IO.puts("CONN OK\n")

        case :ahttp_client.request(conn, ~c"GET", ~c"/json?lat=38.907192&lng=-77.036873", [], nil) do
          {:ok, conn, ref} ->
            IO.puts("REQ OK\n")
            IO.puts("Connection established, receiving response\n")
            receive_response(conn, ref, [])

          error ->
            IO.puts("Request failed: #{inspect(error)}\n")
        end

      error ->
        IO.puts("CONN FAILED\n")
        IO.puts("Connection failed: #{inspect(error)}\n")
    end

    ok = :ssl.stop()
  end

  defp receive_response(conn, ref, acc) do
    case :ahttp_client.recv(conn, 0) do
      {:ok, conn, responses} ->
        process_responses(conn, ref, responses, acc)

      error ->
        IO.puts("Receive failed: #{inspect(error)}\n")
    end
  end

  defp process_responses(conn, ref, [], acc) do
    :ahttp_client.close(conn)

    case acc do
      [data | _] ->
        IO.puts("Data: #{inspect(data)}\n")

      _ ->
        :ok
    end
  end

  defp process_responses(conn, ref, [{:status, ref, status} | rest], acc) do
    IO.puts("Status: #{status}\n")
    process_responses(conn, ref, rest, acc)
  end

  defp process_responses(conn, ref, [{:header, ref, {name, value}} | rest], acc) do
    IO.puts("Header: #{name}: #{value}\n")
    process_responses(conn, ref, rest, acc)
  end

  defp process_responses(conn, ref, [{:data, ref, data} | rest], acc) do
    process_responses(conn, ref, rest, [data | acc])
  end

  defp process_responses(conn, ref, [{:done, ref} | rest], acc) do
    process_responses(conn, ref, rest, acc)
  end

  defp process_responses(conn, ref, [_response | rest], acc) do
    process_responses(conn, ref, rest, acc)
  end
end
