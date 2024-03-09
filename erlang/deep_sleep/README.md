<!---
  Copyright 2023 Fred Dushin <fred@dushin.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# `deep_sleep` Application

Welcome to the `deep_sleep` AtomVM application.

The `deep_sleep` AtomVM application will put the ESP32 device into low-power deep sleep for 10 seconds, wake up, and report the reset reason.  In most cases this will be `esp_rst_deepsleep`, but the initial reset reason will be `esp_rst_poweron`, or if you reset the device manually during sleep.

For more information about programming on the AtomVM platform, see the [AtomVM Programmers Guide](https://www.atomvm.net/doc/master/programmers-guide.html).

For general information about building and executing Erlang AtomVM example programs, see the Erlang example program [README](../README.md).
