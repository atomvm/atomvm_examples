# `gpio_interrupt` Application

Welcome to the `gpio_interrupt` AtomVM application.

The `gpio_interrupt` AtomVM application demonstrates the use of interrupts on GPIO pins.

This application will wait in a loop for interrupt signals when GPIO pin 2 is rising.  This pin is configured to be ordinarily pulled down.  If you momentarily short pin 2 to a 3.3v source, the interrupt will be triggered on the rising side of the signal.

> IMPORTANT.  Do not connect pin 2 to a power source that is higher than the allowable voltage supported by the device (typically, +3.6v).

    Waiting for interrupt ... Interrupt on pin 2
    Waiting for interrupt ... Interrupt on pin 2
    Waiting for interrupt ... Interrupt on pin 2
    Waiting for interrupt ... Interrupt on pin 2
    Waiting for interrupt ... Interrupt on pin 2
    Waiting for interrupt ... Interrupt on pin 2
    Waiting for interrupt ... Interrupt on pin 2
    Waiting for interrupt ...

For more information about programming on the AtomVM platform, see the [AtomVM Programmers Guide](https://www.atomvm.net/doc/master/programmers-guide.html).

For general information about building and executing Erlang AtomVM example programs, see the Erlang example program [README](../README.md).
