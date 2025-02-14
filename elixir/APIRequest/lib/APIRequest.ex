#
# This file is part of AtomVM.
#
# Copyright 2018 Davide Bettio <davide@uninstall.it>
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

defmodule APIRequest do
  def start() do
    :io.format(~c"starting API Request application~n")
    spawn(fn -> start_network() end)

    case wait_for_network() do
      :ok ->
        :io.format(~c"Connected to WiFi! Making HTTP request...~n")

      {:error, reason} ->
        :io.format(~c"Failed to connect to WiFi: ~p~n", [reason])
    end
  end

  defp start_network() do
    :io.format(~c"Starting network...~n")

    config = [
      sta: [
        connected: fn ->
          :io.format(~c"WiFi connected~n")
          spawn(fn -> make_request() end)
        end,
        got_ip: fn info -> :io.format(~c"Got IP: ~p~n", [info]) end,
        disconnected: fn -> :io.format(~c"WiFi disconnected~n") end,
        # Edit these values for your network:
        ssid: ~c"SSID_NAME",
        psk: ~c"SSID_PASSWORD"
      ]
    ]

    case verify_platform(:atomvm.platform()) do
      :ok ->
        :io.format(~c"Starting network...~n")

        case :network.start(config) do
          {:ok, _pid} ->
            :io.format(~c"Network started.~n")
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
    :io.format(~c"Waiting for network...~n")

    receive do
      {:network, :got_ip, _info} -> :ok
    after
      30_000 -> {:error, :timeout}
    end
  end

  defp make_request() do
    :io.format(~c"Making HTTPS request to api.sunrisesunset.io API...~n")

    ssl_opts = [{:active, false}, {:verify, :verify_none}]
    ok = :ssl.start()

    case :ahttp_client.connect(:https, ~c"api.sunrisesunset.io", 443, ssl_opts) do
      {:ok, conn} ->
        case :ahttp_client.request(conn, ~c"GET", ~c"/json?lat=38.907192&lng=-77.036873", [], nil) do
          {:ok, conn, ref} ->
            :io.format(~c"Connection established, receiving response~n")
            receive_response(conn, ref, [])

          error ->
            :io.format(~c"Request failed: ~p~n", [error])
        end

      error ->
        :io.format(~c"Connection failed: ~p~n", [error])
    end

    ok = :ssl.stop()
  end

  defp receive_response(conn, ref, acc) do
    case :ahttp_client.recv(conn, 0) do
      {:ok, conn, responses} ->
        process_responses(conn, ref, responses, acc)

      error ->
        :io.format(~c"Receive failed: ~p~n", [error])
    end
  end

  defp process_responses(conn, ref, [], acc) do
    :ahttp_client.close(conn)

    case acc do
      [data | _] ->
        :io.format(~c"Data: ~p~n", [data])

      _ ->
        :ok
    end
  end

  defp process_responses(conn, ref, [{:status, ref, status} | rest], acc) do
    :io.format(~c"Status: ~p~n", [status])
    process_responses(conn, ref, rest, acc)
  end

  defp process_responses(conn, ref, [{:header, ref, {name, value}} | rest], acc) do
    :io.format(~c"Header: ~s: ~s~n", [name, value])
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
