# `esp_nvs` Application

Welcome to the `esp_nvs` AtomVM application.

The `esp_nvs` AtomVM application uses the ESP non-volatile storage to record the number of times the device has restarted.  If the NVS storage has not been set for this application, it will be intialized with a count of 0.  The device will then sleep for 10 seconds, and restart.  After each restart, the number of restarts is incremented and stored in non-volatile storage.

For more information about programming on the AtomVM platform, see the [AtomVM Programmers Guide](https://doc.atomvm.net/programmers-guide.html).

## Supported Platforms

| Platform | Supported |
|----------|-----------|
| `esp32`  | ✅ |
| `stm32`  | ❌ |
| `generic_unix`  | ❌ |

For general information about building and executing Erlang AtomVM example programs, see the Erlang example program [README](../README.md).
