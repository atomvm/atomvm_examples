<!---
  Copyright 2018 Davide Bettio <davide@uninstall.it>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# `Blinky` Application

Welcome to the `Blinky` AtomVM application.

The `Blinky` AtomVM application will blink an LED attached to pin 2 (`stm32` pin `{:b, 0}`) on and off once every second.

> Note.  This example program only runs on the `esp32`,`stm32`, and `pico` platforms.

To run this example, depending on how bright you want your led, connect a 220 ohm to 1k ohm resistor in series with a 3.3v LED between IO pin 2 and GND.

    +-----------+
    |           |    330 ohm
    |       IO2 o--- \/\/\/\ ---+
    |           |   resistor    |
    |           |               |
    |           |               |
    |           |               |
    |       GND o------|<-------+
    +-----------+      LED
        ESP32

> ESP32 Note.  Many ESP32 development boards already attach pin 2 to an on-board LED.

> STM32 Note. The Nucleo line of boards all have three leds on `{:b, [0, 7, 14]}`.

> Pico-W Note. To use the onboard LED on a `picow` edit lib/Blinky.ex and comment out the current `@pin` definition (change to: `# @pin 2`), and uncomment the definition for the picow onboard LED `@pin` definition: `@pin {:wl, 0}`.

For more information about programming on the AtomVM platform, see the [AtomVM Programmers Guide](https://doc.atomvm.org/latest/programmers-guide.html).

For general information about building and executing Elixir AtomVM example programs, see the Elixir example program [README](../README.md).
