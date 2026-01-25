<!---
  Copyright 2018-2024 Fred Dushin <fred@dushin.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM Elixir Example Programs

Welcome to the AtomVM Elixir Example programs.

The applications in this directory make use of the [AtomVM Mix Plugin](https://github.com/atomvm/ExAtomVM) to compile, assemble, and if applicable, flash the application onto an ESP32 device.

Some example programs can only run on specific platform.  The following table summarizes the platforms on which the Elixir example programs can be run.

| Example Program | esp32 | stm32 | Pico | Pico W | generic_unix |
|-----------------|-------|-------|------|--------|--------------|
| Blinky          | ✅ | ✅ | ✅ | ✦ | ❌ |
| HelloWorld      | ✅ | ✅ | ✅ | ✅ | ✅ |
| LEDC_Example    | ✅ | ❌ | ❌ | ❌ | ❌ |
| WifiExample     | ✅ | ❌ | ❌ | ✅ | ❌ |

✦ Works, but requires editing to use onboard LED, see the README in the examples directory.

To build and run an example in this directory, change your working directory to the corresponding example program, and execute the generic instructions below.

> WARNING!  The Elixir examples in this repository are currently under construction.  They may not run or even compile.  Stay tuned for updates!

## Generic Instructions

The following generic instructions apply to the Elixir tests in this repository.  Special notes about building and running the example programs that deviate from these instructions are noted in the README file for the particular example program.

### Preparation (Optional)

In order to avoid warnings from the Elixir compiler, you can make all of the symbols used from AtomVM libraries available to your application at build time.  This has the advantage of making the compiler less noisy.  However, it has the side effect of making your application files larger than they need to be, which can increase the time to deploy your applications to flash storage, for example, on a device.

If you want to take this path, create a directory called `avm_deps` in the top level of this project directory:

    shell$ mkdir avm_deps

Download a copy of the AtomVM library (`atomvmlib-<version>.avm`) from the AtomVM Github [release repository](https://github.com/atomvm/AtomVM/releases/).  Copy this file into the `avm_deps` directory.

Afterwards, you should see something like:

    shell$ ls -l avm_deps
    total 264
    -rw-rw-r--  1 frege  wheel  11380 May  8 16:32 atomvmlib-v0.6.0.avm

### Building

To build and package this application into an AtomVM AVM file, use the `packbeam` target:

    shell$ mix deps.get
    shell$ mix atomvm.packbeam

This target will create an AVM file (e.g., `Blinky.avm`) file in the top-level directory.

### Running on the ESP32 platform

To run this application on the ESP32 platform, you must flash the application to the device attached to your computer via USB.  You may then optionally monitor the program via a serial console program to view any data output to the console.

#### Flashing onto an ESP32 Device

To flash this application to your ESP32 device, issue the `esp32_flash` target.  Use the `--port` option to specify the port to which your device is connected:

    shell$ mix atomvm.esp32.flash --port /dev/ttyUSB0

#### Monitoring an ESP32 Device

Use a serial console program, such as `minicom`, to attach to the device over USB:

    shell$ minicom -D /dev/ttyUSB0

### Running on the STM32 platform

TODO

### Running on the Raspberry Pico platform

TODO

### Running on Generic Unix platforms

To run this application on a generic UNIX platform, supply the path to the generated AVM file, together with the AtomVM core library

    shell% /path/to/AtomVM ./<example-program>.avm /path/to/atomvmlib.avm

where `<example-program>` is the name of the example program.

> Note.  Currently, you must build the AtomVM virtual machine from source, in order to run AtomVM applications on the generic UNIX platform.
