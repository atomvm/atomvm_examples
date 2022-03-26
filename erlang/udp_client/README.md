# `udp_client` Application

Welcome to the `udp_client` AtomVM application.

The `udp_client` AtomVM application will send UDP packets to a UDP endpoint, once every second.

If the UDP server is running on a separate host from the client application, you must first edit the `src/config.erl` file to set the host and port of the listening server:

    host =>  {192, 168, 4, 1},,
    port => 44404,

> **IMPORTANT** If you are running this example program on an ESP32 device, you must first edit the `src/config.erl` file to set the WiFi Access Point SSID and PSK to which the ESP32 device is to connect before building this application:

    sta => [
        {ssid, "my_sta_ssid"},
        {psk, "my_sta_psk"}
    ]

For more information about programming on the AtomVM platform, the [AtomVM Programmers Guide](https://doc.atomvm.net/programmers-guide.html).

For general information about building and executing AtomVM example programs, see the [Example Program README](../README.md).

You can use the `udp_server` example program to listen for UDP packets from your running application.  Alternatively, you can use a program such as the [netcat](https://en.wikipedia.org/wiki/Netcat) utility to receive UDP packets from the command line.

## Building

To build and package this application into an AtomVM AVM file, use the `packbeam` target:

    shell$ rebar3 packbeam

This target will create the `udp_client.avm` file in the `./_build/default/lib/` directory.

## Running on the ESP32 platform

To run this application on the ESP32 platform, you must flash the application to the device attached to your computer via USB.  You may then optionally monitor the program via a serial console program to view any data output to the console.

### Flashing onto an ESP32 Device

To flash this application to your ESP32 device, issue the `esp32_flash` target.  Use the `--port` option to specify the port to which your device is connected:

    shell$ rebar3 esp32_flash --port /dev/ttyUSB0

### Monitoring an ESP32 Device

Use a serial console program, such as `minicom`, to attach to the device over USB:

    shell$ minicom -D /dev/ttyUSB0

## Running on Generic Unix platforms

To run this application on a generic UNIX platform, supply the path to the generated AVM file, together with the AtomVM core library

    shell% /path/to/AtomVM ./_build/default/lib/udp_client.avm /path/to/atomvmlib.avm

> Note.  Currently, you must build the AtomVM virtual machine from source, in order to run AtomVM applications on the generic UNIX platform.
