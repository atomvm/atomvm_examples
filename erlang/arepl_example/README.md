# `arepl_example` Application

Welcome to the `arepl_example` AtomVM application.

The `arepl_example` AtomVM application demonstrates the use of the `arepl` LISP interpreter.

For more information about programming on the AtomVM platform, see the [AtomVM Programmers Guide](https://doc.atomvm.net/programmers-guide.html).

## Build and Run Instructions

For general information about building and executing Erlang AtomVM example programs, see the Erlang example program [README](../README.md).

> **IMPORTANT** If you are running this example program on a device that supports WiFi (e.g., the ESP32 or Pico W), you must first copy the `src/config.erl-template` file to set `src/config.erl` and edit the WiFi Access Point SSID and PSK to which the ESP32 device is to connect before building this application:

    sta => [
        {ssid, "my_sta_ssid"},
        {psk, "my_sta_psk"}
    ]
