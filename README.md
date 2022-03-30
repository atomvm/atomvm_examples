# AtomVM Example Programs

Welcome to the AtomVM Example Programs repository!

This repository contains a small but growing collection of self-contained example programs demonstrate features of the [AtomVM](https://github.com/atomvm/AtomVM) Virtual Machine.  Examples are provided for both the Erlang and Elixir programming languages.

Each example in this repository represents a self-contained AtomVM "application", which can be built and executed by the AtomVM virtual machine using a minimal set of tooling and to get you up to speed as quickly as possible.

> The example applications in this repository are written for demonstration purposes only and may take short-cuts when it comes to error checking, performance, and beauty.  Use your best judgement about how to integrate these example programs into your applications.

## Requirements

The example programs in this collection require the following:

* A computer running MacOS, Linux, or FreeBSD
* [Erlang/OTP](https://www.erlang.org) Version 21, 22, or 23
* Erlang applications:
    * [rebar3](https://rebar3.readme.io)
* Elixir applications:
    * [Elixir](https://elixir-lang.org) Version 1.13 (version compatible with OTP version selected above)

For instructions about how to install these requirements, consult your local operating system documentation and package management system.

### ESP32 Requirements

In order to flash and run these programs on an ESP32 device, you will need, in addition:

* An ESP32 device with support for USB connectivity via UART, such as the Espressif [ESP32-DevKitC](https://www.espressif.com/en/products/devkits/esp32-devkitc).
* Installation of the release of the AtomVM virtual machine image on the ESP32 device.  For information about how to install a release of the AtomVM virtual machine image on an ESP32 device, see the AtomVM [Getting Started Guide](https://doc.atomvm.net/getting-started-guide.html) documentation.
* A USB cable to connect your ESP32 device.
* The [esptool.py](https://github.com/espressif/esptool) flashing tool.
* (Recommended) A serial console program, such as [`minicom`](https://en.wikipedia.org/wiki/Minicom).
* Some example applications require the presence of a WiFi Access Point (AP), to which the ESP32 device can connect.
* Some example applications require either an existing WiFi Access Point (AP) or that your personal computer be capable of connecting to the ESP32 acting as a WiFi Access Point (AP).

> Note. AtomVM is currently qualified only on the [ESP32](https://www.espressif.com/en/products/socs/esp32) SoC.  Support for the ESP32-C and ESP32-S* SoCs is TBD.

### STM32 Requirements

TODO

### Generic Unix Requirements

No additional requirements are needed, other than the above requirements, to build and test the example programs in this repository on the generic UNIX platform.
