
# AtomVM Erlang Example Programs

Welcome to the AtomVM Erlang Example programs.


The applications in this directory make use of the [AtomVM Rebar3 Plugin](https://github.com/atomvm/atomvm_rebar3_plugin) to compile, assemble, and if applicable, flash the application onto an ESP32 device.

For each example in this directory, change your working directory to the corresponding example program, and execute the instructions in the `README.md` in each example.

## Requirements

The example programs in this collection require the following:

* A computer running MacOS, Linux, or FreeBSD
* [Erlang/OTP](https://www.erlang.org) (version 21, 22, or 23)
* [rebar3](https://rebar3.readme.io)

For instructions about how to install these requirements, consult your local operating system documentation and package management system.

### ESP32 Requirements

In order to flash and run these programs on an ESP32 device, you will need, in addition:

* An ESP32 device with support for USB connectivity via UART, such as the Espressif [ESP32-DevKitC](https://www.espressif.com/en/products/devkits/esp32-devkitc).
* A USB cable to connect your ESP32 device.
* The [esptool.py](https://github.com/espressif/esptool) flashing tool.
* (Recommended) A serial console program, such as [`minicom`](https://en.wikipedia.org/wiki/Minicom).
* Some example applications require the presence of a WiFi Access Point (AP), to which the ESP32 device can connect.
* Some example applications require either an existing WiFi Access Point (AP) or that your personal computer be capable of connecting to the ESP32 acting as a WiFi Access Point (AP).

> Note. AtomVM is currently qualified only on the [ESP32](https://www.espressif.com/en/products/socs/esp32) SoC.  Support for the ESP32-C and ESP32-S* SoCs is TBD.
