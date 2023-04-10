
# AtomVM Erlang Example Programs

Welcome to the AtomVM Erlang Example programs.

The applications in this directory make use of the [AtomVM Rebar3 Plugin](https://github.com/atomvm/atomvm_rebar3_plugin) to compile, assemble, and if applicable, flash the application onto an ESP32 device.

Some example programs can only run on specific platform.  The following table summarizes the platforms on which the Erlang example programs can be run.

| Example Program | esp32 | stm32 | generic_unix |
|-----------------|-------|-------|--------------|
| arepl_example   | ✅ | ❌ | ✅ |
| blinky          | ✅ | ✅ | ❌ |
| deep_sleep      | ✅ | ❌ | ❌ |
| esp_nvs         | ✅ | ❌ | ❌ |
| gpio_interrupt  | ✅ | ✅ | ❌ |
| hello_world     | ✅ | ✅ | ✅ |
| http_server_example | ✅ | ❌ | ✅ |
| i2c_example     | ✅ | ❌ | ❌ |
| ledc_example    | ✅ | ❌ | ❌ |
| read_priv       | ✅ | ✅ | ✅ |
| spi_example     | ✅ | ❌ | ❌ |
| system_info     | ✅ | ✅ | ✅ |
| tcp_client      | ✅ | ❌ | ✅ |
| tcp_server      | ✅ | ❌ | ✅ |
| uart_example    | ✅ | ❌ | ❌ |
| udp_client      | ✅ | ❌ | ✅ |
| udp_server      | ✅ | ❌ | ✅ |
| wifi            | ✅ | ❌ | ❌ |

To build and run an example in this directory, change your working directory to the corresponding example program, and execute the generic instructions below.

## Generic Instructions

The following generic instructions apply to the Erlang examples in this repository.  Special notes about building and running the example programs that deviate from these instructions are noted in the README file for the particular example program.

### Building

To build and package this application into an AtomVM AVM file, use the `packbeam` target:

    shell$ rebar3 packbeam -p -f -i

This target will create the `read_priv.avm` file in the `./_build/default/lib/` directory.

### Running on the ESP32 platform

To run this application on the ESP32 platform, you must flash the application to the device attached to your computer via USB.  You may then optionally monitor the program via a serial console program to view any data output to the console.

#### Flashing onto an ESP32 Device

To flash this application to your ESP32 device, issue the `esp32_flash` target.  Use the `--port` option to specify the port to which your device is connected:

    shell$ rebar3 esp32_flash --port /dev/ttyUSB0

#### Monitoring an ESP32 Device

Use a serial console program, such as `minicom`, to attach to the device over USB:

    shell$ minicom -D /dev/ttyUSB0

### Running on the STM32 platform

TODO

### Running on Generic Unix platforms

To run this application on a generic UNIX platform, supply the path to the generated AVM file, together with the AtomVM core library

    shell% /path/to/AtomVM ./_build/default/lib/<example-program>.avm /path/to/atomvmlib.avm

where `<example-program>` is the name of the example program.

> Note.  Currently, you must build the AtomVM virtual machine from source, in order to run AtomVM applications on the generic UNIX platform.
