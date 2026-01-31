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

defmodule EspNvs do
  @moduledoc """
  Demonstrates ESP32 NVS usage by storing a small value once.
  """

  @compile {:no_warn_undefined, :atomvm}
  @compile {:no_warn_undefined, :esp}

  @nvs_namespace :esp_nvs
  @nvs_key :word
  @default_word "Hello"
  @restart_interval_ms 10_000

  def start do
    case verify_platform(:atomvm.platform()) do
      :ok ->
        case :esp.nvs_fetch_binary(@nvs_namespace, @nvs_key) do
          {:ok, word} when is_binary(word) ->
            IO.puts("Fetched word from NVS: #{word}")

          {:error, :not_found} ->
            IO.puts("No word found. Storing default word to NVS once: #{@default_word}")
            :ok = :esp.nvs_put_binary(@nvs_namespace, @nvs_key, @default_word)

          _ ->
            IO.puts("Error reading NVS. Skipping write.")
        end

        IO.puts("Restarting in 10 seconds ...")
        Process.sleep(@restart_interval_ms)
        :esp.restart()

      error ->
        error
    end
  end

  defp verify_platform(:esp32), do: :ok
  defp verify_platform(platform), do: {:error, {:unsupported_platform, platform}}
end
