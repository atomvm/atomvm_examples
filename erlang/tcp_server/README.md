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

> **IMPORTANT** If you are running this example program on an ESP32 device, you must first edit the `src/config.erl` file to set the WiFi Access Point SSID and PSK to which the ESP32 device is to connect before building this application:

    sta => [
        {ssid, "my_sta_ssid"},
        {psk, "my_sta_psk"}
    ]

You can use the `tcp_client` example program to send TCP/IP packets to your running application.  Alternatively, you can use a program such as the [netcat](https://en.wikipedia.org/wiki/Netcat) utility to send TCP packets from the command line.

For more information about programming on the AtomVM platform, see the [AtomVM Programmers Guide](https://doc.atomvm.net/programmers-guide.html).

## Supported Platforms

| Platform | Supported |
|----------|-----------|
| `esp32`  | ✅ |
| `stm32`  | ❌ |
| `generic_unix`  | ✅ |

For general information about building and executing Erlang AtomVM example programs, see the Erlang example program [README](../README.md).
