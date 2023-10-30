# `wifi` Application

Welcome to the `wifi` AtomVM application.

The `wifi` AtomVM application demonstrates how to configure an ESP32 device for both Station (STA) and Access Point (AP) modes, allowing an ESP32 device to join an existing WiFi network or to serve as a WiFi access point for other devices.

For more information about programming on the AtomVM platform, see the [AtomVM Programmers Guide](https://www.atomvm.net/doc/master/programmers-guide.html).

For general information about building and executing Erlang AtomVM example programs, see the Erlang example program [README](../README.md).

> **IMPORTANT** If you are running this example program on a device that supports WiFi (e.g., the ESP32 or Pico W), you must first copy the `src/config.erl-template` file to set `src/config.erl` and edit the WiFi Access Point SSID and PSK to which the ESP32 device is to connect before building this application:

    sta => [
        {ssid, "my_sta_ssid"},
        {psk, "my_sta_psk"}
    ]
