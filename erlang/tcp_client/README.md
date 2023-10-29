# `tcp_client` Application

Welcome to the `tcp_client` AtomVM application.

The `tcp_client` AtomVM application will send TCP packets to a TCP endpoint, once every second, and will print the packet received back from the server to the console.

For example:

    Connected to "192.168.4.1:44404" from "192.168.4.2:61155"
    Sent <<"AtomVM rocks!">> to "192.168.4.1:44404"
    Received <<"AtomVM rocks!">> from "192.168.4.1:44404"

If the TCP server is running on a separate host from the client application, you must first edit the `src/config.erl` file to set the host and port of the listening server:

    host => {192, 168, 4, 1},
    port => 44404,

> **IMPORTANT** If you are running this example program on a device that supports WiFi (e.g., the ESP32 or Pico W), you must first copy the `src/config.erl-template` file to set `src/config.erl` and edit the WiFi Access Point SSID and PSK to which the ESP32 device is to connect before building this application:

    sta => [
        {ssid, "my_sta_ssid"},
        {psk, "my_sta_psk"}
    ]

You can use the `tcp_server` example program to listen for TCP packets from your running application.  Alternatively, you can use a program such as the [netcat](https://en.wikipedia.org/wiki/Netcat) utility to receive TCP packets from the command line.

For more information about programming on the AtomVM platform, see the [AtomVM Programmers Guide](https://doc.atomvm.net/programmers-guide.html).

For general information about building and executing Erlang AtomVM example programs, see the Erlang example program [README](../README.md).

> **IMPORTANT** If you are running this example program on a device that supports WiFi (e.g., the ESP32 or Pico W), you must first copy the `src/config.erl-template` file to set `src/config.erl` and edit the WiFi Access Point SSID and PSK to which the ESP32 device is to connect before building this application:

    sta => [
        {ssid, "my_sta_ssid"},
        {psk, "my_sta_psk"}
    ]
