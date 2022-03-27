# `Blinky` Application

Welcome to the `Blinky` AtomVM application.

The `Blinky` AtomVM application will blink an LED attached to pin 2 on and off once every second.

> Note.  This example program only runs on the `esp32` and `stm32` platforms.

For general information about building and executing AtomVM example programs, see the [Example Program README](../README.md).

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

## Building

To build and package this application into an AtomVM AVM file, use the `packbeam` target:

    shell$ mix deps.get
    shell$ mix atomvm.packbeam

This target will create the `Blinky.avm` file in the `./_build/default/lib/` directory.

## Running on the ESP32 platform

To run this application on the ESP32 platform, you must flash the application to the device attached to your computer via USB.  You may then optionally monitor the program via a serial console program to view any data output to the console.

### Flashing onto an ESP32 Device

To flash this application to your ESP32 device, issue the `esp32_flash` target.  Use the `--port` option to specify the port to which your device is connected:

    shell$ mix atomvm.esp32_flash --port /dev/ttyUSB0

### Monitoring an ESP32 Device

Use a serial console program, such as `minicom`, to attach to the device over USB:

    shell$ minicom -D /dev/ttyUSB0

## Running on Generic Unix platforms

To run this application on a generic UNIX platform, supply the path to the generated AVM file, together with the AtomVM core library

    shell% /path/to/AtomVM ./_build/dev/lib/blinky.avm /path/to/atomvmlib.avm

> Note.  Currently, you must build the AtomVM virtual machine from source, in order to run AtomVM applications on the generic UNIX platform.
