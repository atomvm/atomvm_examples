<!---
  Copyright 2023 Fred Dushin <fred@dushin.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# `tcp_server` Application

Welcome to the `tcp_server` AtomVM application.

The `tcp_server` AtomVM application will start a UDP endpoint and listen for TCP packets sent to the host or device.  When a packet is received, the packet will be echoed back to the calling client over the same TCP/IP connection..

For example:

    Listening on "0.0.0.0:44404".
    Waiting to accept connection...
    Accepted connection.  local: "192.168.4.1:44404" peer: "192.168.4.1:61059"
    Waiting to receive data...
    Waiting to accept connection...
    Received packet <<"AtomVM rocks!">> from "192.168.4.2:61059".  Echoing back...

> **IMPORTANT** If you are running this example program on a device that supports WiFi (e.g., the ESP32 or Pico W), you must first copy the `src/config.erl-template` file to set `src/config.erl` and edit the WiFi Access Point SSID and PSK to which the ESP32 device is to connect before building this application:

    sta => [
        {ssid, "my_sta_ssid"},
        {psk, "my_sta_psk"}
    ]

You can use the `tcp_client` example program to send TCP/IP packets to your running application.  Alternatively, you can use a program such as the [netcat](https://en.wikipedia.org/wiki/Netcat) utility to send TCP packets from the command line.

For more information about programming on the AtomVM platform, see the [AtomVM Programmers Guide](https://doc.atomvm.org/latest/programmers-guide.html).

For general information about building and executing Erlang AtomVM example programs, see the Erlang example program [README](../README.md).

> **IMPORTANT** If you are running this example program on a device that supports WiFi (e.g., the ESP32 or Pico W), you must first copy the `src/config.erl-template` file to set `src/config.erl` and edit the WiFi Access Point SSID and PSK to which the ESP32 device is to connect before building this application:

    sta => [
        {ssid, "my_sta_ssid"},
        {psk, "my_sta_psk"}
    ]
