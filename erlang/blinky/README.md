# `blinky` Application

Welcome to the `blinky` AtomVM application.

The `blinky` AtomVM application will blink an LED attached to pin 2 on and off once every second.

> Note.  This example program only runs on the `esp32` and `stm32` platforms.

To run this example, connect a 1k ohm resistor in series with a 3.3v LED between IO pin 2 and GND.

    +-----------+
    |           |    1k ohm
    |       IO2 o--- \/\/\/\ ---+
    |           |   resistor    |
    |           |               |
    |           |               |
    |           |               |
    |       GND o------|<-------+
    +-----------+      LED
        ESP32

> Note.  Many ESP32 development boards already attach pin 2 to an on-board LED.

For more information about programming on the AtomVM platform, see the [AtomVM Programmers Guide](https://doc.atomvm.net/programmers-guide.html).

For general information about building and executing Erlang AtomVM example programs, see the Erlang example program [README](../README.md).
