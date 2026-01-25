<!---
  Copyright 2026 Masatoshi Nishiguchi

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# `Wifi` Application

Welcome to the `Wifi` AtomVM application.

The `Wifi` AtomVM application demonstrates how to configure an ESP32 device for both Station (STA) and Access Point (AP) modes, allowing an ESP32 device to join an existing WiFi network or to serve as a WiFi access point for other devices.

For more information about programming on the AtomVM platform, see the [AtomVM Programmers Guide](https://doc.atomvm.org/latest/programmers-guide.html).

For general information about building and executing Elixir AtomVM example programs, see the Elixir example program [README](../README.md).

> **IMPORTANT** If you are running this example program on a device that
> supports WiFi (e.g., the ESP32), you must first edit `config/config.exs` and
> set the WiFi Station (STA) SSID and PSK before building this application:
>
> ```elixir
> config :wifi,
>   sta: [
>     dhcp_hostname: "my_device_name",
>     ssid: "my_sta_ssid",
>     psk: "my_sta_psk"
>   ]
> ```
