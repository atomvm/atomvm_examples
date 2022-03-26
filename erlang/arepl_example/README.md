# `arepl_example` Application

Welcome to the `arepl_example` AtomVM application.

The `arepl_example` AtomVM application demonstrates the use of the `arepl` LISP interpreter.

For more information about programming on the AtomVM platform, the [AtomVM Programmers Guide](https://doc.atomvm.net/programmers-guide.html).

For general information about building and executing AtomVM example programs, see the [Example Program README](../README.md).

## Building

To build and package this application into an AtomVM AVM file, use the `packbeam` target:

    shell$ rebar3 packbeam

This target will create the `arepl_example.avm` file in the `./_build/default/lib/` directory.

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

    shell% /path/to/AtomVM ./_build/default/lib/arepl_example.avm /path/to/atomvmlib.avm

> Note.  Currently, you must build the AtomVM virtual machine from source, in order to run AtomVM applications on the generic UNIX platform.
