# AtomVM Example Programs

Welcome to the AtomVM Example Programs repository!

This repository contains a small but growing collection of self-contained example programs demonstrate features of the [AtomVM](https://github.com/atomvm/AtomVM) Virtual Machine.  Examples are provided for both the Erlang and Elixir programming languages.

Each example in this repository represents a self-contained AtomVM "application", which can be built and executed by the AtomVM virtual machine using a minimal set of tooling and to get you up to speed as quickly as possible.

> The example applications in this repository are written for demonstration purposes only and may take short-cuts when it comes to error checking, performance, and aesthetics.  Use your best judgement about how to integrate these example programs into your applications.

## Requirements

The example programs in this collection require the following:

* A computer running MacOS, Linux, or FreeBSD
* [Erlang/OTP](https://www.erlang.org) (any version supported by AtomVM)
* Erlang applications:
    * [rebar3](https://rebar3.readme.io)
* Elixir applications:
    * [Elixir](https://elixir-lang.org) Version 1.13 (version compatible with OTP version selected above)

See the [AtomVM Release Notes](https://www.atomvm.net/doc/master/release-notes.html) for information about supported versions of the above software.

For instructions about how to install these requirements, consult your local operating system documentation and package management system.

The above tools make use of plugins designed specifically to simply the development and deployment of applications on to supported platforms.  For more information about these plugins, please refer to the [AtomVM Tooling](https://www.atomvm.net/doc/master/atomvm-tooling.html) documentation.

These instructions assume you have already deployed the AtomVM virtual machine on to the device on which you are running.  For instructions about how to install the AtomVM virtual machine onto devices, see the AtomVM [Getting Started Guide](https://www.atomvm.net/doc/master/getting-started-guide.html).

### ESP32 Requirements

In order to flash and run these programs on an ESP32 device, you will need, in addition:

* An ESP32 device with support for USB connectivity via UART, such as the Espressif [ESP32-DevKitC](https://www.espressif.com/en/products/devkits/esp32-devkitc).
* Installation of the release of the AtomVM virtual machine image on the ESP32 device.  For information about how to install a release of the AtomVM virtual machine image on an ESP32 device, see the AtomVM [Getting Started Guide](https://doc.atomvm.net/getting-started-guide.html) documentation.
* A USB cable to connect your ESP32 device.
* The [esptool.py](https://github.com/espressif/esptool) flashing tool.
* (Recommended) A serial console program, such as [`minicom`](https://en.wikipedia.org/wiki/Minicom).
* Some example applications require the presence of a WiFi Access Point (AP), to which the ESP32 device can connect.
* Some example applications require either an existing WiFi Access Point (AP) or that your personal computer be capable of connecting to the ESP32 acting as a WiFi Access Point (AP).

> For information about supported ESP32 SoCs, see the AtomVM [Release Notes](https://www.atomvm.net/doc/master/release-notes.html).

### STM32 Requirements

In order to flash and run these programs on an STM32 device, you will need, in addition:

* An STM32 device with support for USB connectivity via UART, such as the [STMicroelectronics](https://www.st.com) [Nucleo-F429ZI](https://www.st.com/en/evaluation-tools/nucleo-f429zi.html).  (Some STM32 boards require an external adapter for UART connectivity)
* Installation of the release of the AtomVM virtual machine image on the STM32 device.  For information about how to install a release of the AtomVM virtual machine image on an STM32 device, see the AtomVM [Getting Started Guide](https://doc.atomvm.net/getting-started-guide.html) documentation.
* A USB cable to connect to UART and/or JTAG. (The [STM32F4Discovery](https://www.st.com/en/evaluation-tools/stm32f4discovery.html) needs two cables -- one for built in JTAG, and one for external UART, where as on the [Nucleo-F429ZI](https://www.st.com/en/evaluation-tools/nucleo-f429zi.html) board both UART and JTAG are presented on the one on-board usb connection).
* The [st-flash](https://github.com/texane/stlink) flashing tool.
* To use JTAG for flashing and console output debugging, you will need a [st-link v2](https://www.st.com/en/development-tools/st-link-v2.html) or [st-link v3](https://www.st.com/en/development-tools/stlink-v3set.html) device (typically already included on Nucleo and Discovery boards).
* (Recommended) A serial console program, such as [`minicom`](https://en.wikipedia.org/wiki/Minicom).

> For information about supported STM32 development boards, see the AtomVM [Release Notes](https://www.atomvm.net/doc/master/release-notes.html).

### Raspberry Pico Requirements

In order to flash and run these programs on a Raspberry Pico or Raspberry Pico W device, you will need, in addition:

* A [Raspberry Pico](https://www.raspberrypi.com/documentation/microcontrollers/raspberry-pi-pico.html#raspberry-pi-pico-and-pico-h) or [Raspberry Pico W](https://www.raspberrypi.com/documentation/microcontrollers/raspberry-pi-pico.html#raspberry-pi-pico-w-and-pico-wh) device with support for USB connectivity.
* Installation of the release of the AtomVM virtual machine image on the Raspberry Pico device.  For information about how to install a release of the AtomVM virtual machine image on an ESP32 device, see the AtomVM [Getting Started Guide](https://doc.atomvm.net/getting-started-guide.html) documentation.
* A USB cable to connect your Raspberry Pico device.
* (Recommended) A serial console program, such as [`minicom`](https://en.wikipedia.org/wiki/Minicom).
* Some example applications require the presence of a WiFi Access Point (AP), to which the Raspberry Pico W device can connect.  Note that these examples are only supported on the Raspberry Pico W.

> For information about supported Raspberry Pico devices, see the AtomVM [Release Notes](https://www.atomvm.net/doc/master/release-notes.html).

### Generic Unix Requirements

No additional requirements are needed, other than the above requirements, to build and test the example programs in this repository on the generic UNIX platform.

## Status

This repo uses Github Workflows to run some basic source code checks and build tests.

[![Build and Test](https://github.com/atomvm/atomvm_examples/actions/workflows/build-examples.yaml/badge.svg?branch=master)](https://github.com/atomvm/atomvm_examples/actions/workflows/build-examples.yaml)
