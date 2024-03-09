<!---
  Copyright 2023 Fred Dushin <fred@dushin.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# `udp_server` Application

Welcome to the `udp_server` AtomVM application.

The `udp_server` AtomVM application will start a UDP endpoint and listen for UDP packets sent to the host or device.  When a packet is received, the packet and sending host will be printed to the console.

For example:

    Opened UDP socket on "0.0.0.0:44404".
    Waiting to receive data...

    Received UDP packet <<"AtomVM rocks!">> from "192.168.4.2:51277"

You can use the `udp_client` example program to send UDP packets to your running application.  Alternatively, you can use a program such as the [netcat](https://en.wikipedia.org/wiki/Netcat) utility to send UDP packets from the command line.

For more information about programming on the AtomVM platform, see the [AtomVM Programmers Guide](https://www.atomvm.net/doc/master/programmers-guide.html).

For general information about building and executing Erlang AtomVM example programs, see the Erlang example program [README](../README.md).

> **IMPORTANT** If you are running this example program on a device that supports WiFi (e.g., the ESP32 or Pico W), you must first copy the `src/config.erl-template` file to set `src/config.erl` and edit the WiFi Access Point SSID and PSK to which the ESP32 device is to connect before building this application:

    sta => [
        {ssid, "my_sta_ssid"},
        {psk, "my_sta_psk"}
    ]
