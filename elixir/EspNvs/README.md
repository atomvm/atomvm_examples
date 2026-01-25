<!---
  Copyright 2026 Masatoshi Nishiguchi

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# `EspNvs` Application

Welcome to the `EspNvs` AtomVM application.

The `EspNvs` AtomVM application uses ESP32 non-volatile storage (NVS) to store a small value.
If the NVS storage has not been set for this application, it will store a default value once. The application then logs the result and restarts every 10 seconds.

For more information about programming on the AtomVM platform, see the [AtomVM Programmers Guide](https://doc.atomvm.org/latest/programmers-guide.html).

For general information about building and executing Elixir AtomVM example programs, see the Elixir example program [README](../README.md).
