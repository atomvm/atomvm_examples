# `deep_sleep` Application

Welcome to the `deep_sleep` AtomVM application.

The `deep_sleep` AtomVM application will put the ESP32 device into low-power deep sleep for 10 seconds, wake up, and report the reset reason.  In most cases this will be `esp_rst_deepsleep`, but the initial reset reason will be `esp_rst_poweron`, or if you reset the device manually during sleep.

For more information about programming on the AtomVM platform, see the [AtomVM Programmers Guide](https://doc.atomvm.net/programmers-guide.html).

For general information about building and executing Erlang AtomVM example programs, see the Erlang example program [README](../README.md).
