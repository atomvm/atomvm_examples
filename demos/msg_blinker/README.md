# `msg_blinker` Application

Welcome to the `msg_blinker` AtomVM application.

The `msg_blinker` AtomVM application will start two processes. The sender process will send an on and off message to the receiver process every 2 seconds. The receiver process with blink an LED attached to pin 2 on and off depending on whether it receives an on or off message.

> Note.  This example program has only been tested to run on the `esp32` platform.

To run this example, depending on how bright you want your led, connect a 220 ohm to 1k ohm resistor in series with a 3.3v LED between IO pin 2 and GND.

    +-----------+
    |           |    330 ohm
    |       IO2 o--- \/\/\/\ ---+
    |           |   resistor    |
    |           |               |
    |           |               |
    |           |               |
    |       GND o------|<-------+
    +-----------+      LED
        ESP32

> ESP32 Note.  Many ESP32 development boards already attach pin 2 to an on-board LED.

## Supported Platforms

| Platform       | Supported |
|----------------|-----------|
| `esp32`        |  &check;  |
| `stm32`        |  &cross;  |
| `generic_unix` |  &cross;  |

For more information about programming on the AtomVM platform, see the [AtomVM Programmers Guide](https://www.atomvm.net/doc/master/programmers-guide.html).

For general information about building and executing Erlang AtomVM example programs, see the Erlang example program [README](../README.md).
