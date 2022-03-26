# `gpio_interrupt` Application

Welcome to the `gpio_interrupt` AtomVM application.

The `gpio_interrupt` AtomVM application demonstrates the use of interrupts on GPIO pins.

This application will wait in a loop for interrupt signals when GPIO pin 2 is rising.  This pin is configured to be ordinarily pulled down.  If you momentarily short pin 2 to a 3.3v source, the interrupt will be triggered on the rising side of the signal.

> IMPORTANT.  Do not connect pin 2 to a power source that is higher than the allowable voltage supported by the device (typically, +3.6v).

    ###########################################################

       ###    ########  #######  ##     ## ##     ## ##     ##
      ## ##      ##    ##     ## ###   ### ##     ## ###   ###
     ##   ##     ##    ##     ## #### #### ##     ## #### ####
    ##     ##    ##    ##     ## ## ### ## ##     ## ## ### ##
    #########    ##    ##     ## ##     ##  ##   ##  ##     ##
    ##     ##    ##    ##     ## ##     ##   ## ##   ##     ##
    ##     ##    ##     #######  ##     ##    ###    ##     ##

    ###########################################################

    I (311) AtomVM: Starting AtomVM revision 0.5.0
    I (311) AtomVM: Loaded BEAM partition main.avm at address 0x210000 (size=1048576 bytes)
    I (371) AtomVM: Found startup beam gpio_interrupt.beam
    I (371) AtomVM: Loaded BEAM partition lib.avm at address 0x1d0000 (size=262144 bytes)
    I (371) AtomVM: Starting gpio_interrupt.beam...
    ---
    Waiting for interrupt ... Interrupt on pin 2
    Waiting for interrupt ... Interrupt on pin 2
    Waiting for interrupt ... Interrupt on pin 2
    Waiting for interrupt ... Interrupt on pin 2
    Waiting for interrupt ... Interrupt on pin 2
    Waiting for interrupt ... Interrupt on pin 2
    Waiting for interrupt ... Interrupt on pin 2
    Waiting for interrupt ...

For more information about programming on the AtomVM platform, the [AtomVM Programmers Guide](https://doc.atomvm.net/programmers-guide.html).

For general information about building and executing AtomVM example programs, see the [Example Program README](../README.md).

## Building

To build and package this application into an AtomVM AVM file, use the `packbeam` target:

    shell$ rebar3 packbeam

This target will create the `gpio_interrupt.avm` file in the `./_build/default/lib/` directory.

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

    shell% /path/to/AtomVM ./_build/default/lib/gpio_interrupt.avm /path/to/atomvmlib.avm

> Note.  Currently, you must build the AtomVM virtual machine from source, in order to run AtomVM applications on the generic UNIX platform.
