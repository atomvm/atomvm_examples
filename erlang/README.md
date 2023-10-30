
# AtomVM Erlang Example Programs

Welcome to the AtomVM Erlang Example programs.

The applications in this directory make use of the [AtomVM Rebar3 Plugin](https://github.com/atomvm/atomvm_rebar3_plugin) to compile, assemble, and if applicable, flash the application onto an ESP32 device.

Some example programs can only run on specific platform.  The following table summarizes the platforms on which the Erlang example programs can be run.

| Example Program | esp32 | stm32 | Pico | Pico W | generic_unix |
|-----------------|-------|-------|----------------|------------------|--------------|
| arepl_example   | ✅ | ❌ | ❌ | ❌ | ✅ |
| blinky          | ✅ | ✅ | ✅ | ✦ | ❌ |
| deep_sleep      | ✅ | ❌ | ❌ | ❌ | ❌ |
| esp_nvs         | ✅ | ❌ | ❌ | ❌ | ❌ |
| gpio_interrupt  | ✅ | ✅ | ❌ | ❌ | ❌ |
| hello_world     | ✅ | ✅ | ✅ | ✅ | ✅ |
| http_server_example | ✅ | ❌ | ❌ | ❌ | ✅ |
| i2c_example     | ✅ | ❌ | ❌ | ❌ | ❌ |
| ledc_example    | ✅ | ❌ | ❌ | ❌ | ❌ |
| read_priv       | ✅ | ✅ | ✅ | ✅ | ✅ |
| spi_example     | ✅ | ❌ | ❌ | ❌ | ❌ |
| system_info     | ✅ | ✅ | ✅ | ✅ | ✅ |
| tcp_client      | ✅ | ❌ | ❌ | ❌ | ✅ |
| tcp_server      | ✅ | ❌ | ❌ | ❌ | ✅ |
| uart_example    | ✅ | ❌ | ❌ | ❌ | ❌ |
| udp_client      | ✅ | ❌ | ❌ | ❌ | ✅ |
| udp_server      | ✅ | ❌ | ❌ | ❌ | ✅ |
| wifi            | ✅ | ❌ | ❌ | ❌ | ❌ |

✦ Works, but requires editing to use onboard LED, see the README in the examples directory.

To build and run an example in this directory, change your working directory to the corresponding example program, and execute the generic instructions below.

## Generic Instructions

The following generic instructions apply to the Erlang examples in this repository.  Special notes about building and running the example programs that deviate from these instructions are noted in the README file for the particular example program.

These instructions make use of the [`atomvm_rebar3_plugin`](https://atomvm.github.io/atomvm_rebar3_plugin).  For more information about AtomVM tooling, please see the [AtomVM Tooling](https://www.atomvm.net/doc/master/atomvm-tooling.html) documentation.

### Building

To build and package this application into an AtomVM AVM file, use the `packbeam` target:

    shell$ rebar3 atomvm packbeam

This target will create the `<application>.avm` file in the `./_build/default/lib/` directory, where `<application>` is the name of the example application.

### Running on the ESP32 platform

To run this application on the ESP32 platform, you must flash the application to the device attached to your computer via USB.  You may then optionally monitor the program via a serial console program to view any data output to the console.

#### Flashing onto an ESP32 Device

To flash this application to your ESP32 device, issue the `esp32_flash` target.  Use the `--port` option to specify the port to which your device is connected:

    shell$ rebar3 atomvm esp32_flash --port /dev/ttyUSB0

#### Monitoring an ESP32 Device

Use a serial console program, such as `minicom`, to attach to the device over USB:

    shell$ minicom -D /dev/ttyUSB0

### Running on the STM32 platform

To run this application on the STM32 platform, you must flash the application to the device attached to your computer via USB.  You may then optionally monitor the program via a serial console program to view any data output to the console.

#### Flashing onto an STM32 Device

The STM32 platform requires that the core AtomVM library be included in the application you are targeting for the device.  For information about how to obtain this library, see the [AtomVM Tooling](https://www.atomvm.net/doc/master/atomvm-tooling.html) documentation.

Once you have obtained this library, you must use the `packeam` task to create the AVM file to deploy to your device.  Specify the path to the AtomVM core library using the `-e` option, e.g.,

    shell$ rebar3 atomvm packbeam -e /path/to/atomvmlib.avm

To flash this application to your STM32 device, issue the `stm32_flash` target.  Use the `--port` option to specify the port to which your device is connected:

    shell$ rebar3 atomvm stm32_flash

#### Monitoring an STM32 Device

Use a serial console program, such as `minicom`, to attach to the device over USB:

    shell$ minicom -D /dev/ttyUSB0

### Running on the Raspberry Pico platform

To run this application on the Raspberry Pico platform, you must flash the application to the device attached to your computer via USB.  You may then optionally monitor the program via a serial console program to view any data output to the console.

#### Flashing onto a Raspberry Pico Device

To flash this application to your Pico device, issue the `pico_flash` target:

    shell$ rebar3 atomvm pico_flash

#### Monitoring a Raspberry Pico Device

Use a serial console program, such as `minicom`, to attach to the device over USB:

    shell$ minicom -D /dev/ttyACM0

> Note.  After restart, the Pico device should show up as a device on Linux development hosts under `/dev/ttyACM0`.  On MacOS hosts, the Pico device should show up under a device name that matches `/dev/cu.usbmodem14*`.

### Running on Generic Unix platforms

To run this application on a generic UNIX platform, supply the path to the generated AVM file to the `atomvm` command:

    shell% atomvm ./_build/default/lib/<example-program>.avm

where `<example-program>` is the name of the example program.

> Note.  See the AtomVM [Getting Started Guide](https://www.atomvm.net/doc/master/getting-started-guide.html) for information about how to install AtomVM on the generic UNIX platform.
