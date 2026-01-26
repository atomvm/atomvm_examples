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

defmodule Blinky do
  # Pin 2 works on any pico device with an external LED
  @pin 2
  # Comment out above and uncomment below to use Pico W onboard LED
  # @pin {:wl, 0}

  def start() do
    platform_gpio_setup()
    loop(pin(), :low)
  end

  defp loop(pin, level) do
    :io.format(~c"Setting pin ~p ~p~n", [pin, level])
    GPIO.digital_write(pin, level)
    Process.sleep(1000)
    loop(pin, toggle(level))
  end

  defp toggle(:high) do
    :low
  end

  defp toggle(:low) do
    :high
  end

  defp pin() do
    case :atomvm.platform() do
      :esp32 -> 2
      :pico -> @pin
      :stm32 -> {:b, [0]}
      unsupported -> :erlang.exit({:unsupported_platform, unsupported})
    end
  end

  defp platform_gpio_setup() do
    case :atomvm.platform() do
      :esp32 ->
        GPIO.set_pin_mode(pin(), :output)

      :stm32 ->
        GPIO.set_pin_mode(pin(), :output)

      :pico ->
        case @pin do
          {:wl, 0} ->
            :ok

          pin ->
            GPIO.init(pin)
            GPIO.set_pin_mode(pin, :output)
        end

      unsupported ->
        :io.format("Platform ~p is not supported.~n", [unsupported])
        :erlang.exit({:error, {:unsupported_platform, unsupported}})
    end
  end
end
