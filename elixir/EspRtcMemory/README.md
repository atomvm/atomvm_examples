<!---
  Copyright 2026 Masatoshi Nishiguchi

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# `EspRtcMemory` Application

Welcome to the `EspRtcMemory` AtomVM application.

The `EspRtcMemory` AtomVM application uses ESP32 RTC slow memory to record the number of times the device has restarted.
If no value has been stored in RTC slow memory yet, the counter is initialized to 0. The device will then sleep for 10 seconds and restart. After each restart, the counter is incremented and stored back to RTC slow memory.

For more information about programming on the AtomVM platform, see the [AtomVM Programmers Guide](https://doc.atomvm.org/latest/programmers-guide.html).

For general information about building and executing Elixir AtomVM example programs, see the Elixir example program [README](../README.md).
