# `read_priv` Application

Welcome to the `read_priv` AtomVM application.

The `read_priv` AtomVM application demonstrates the ability to write plain file data to an AVM module, and to read the contents of that file into memory using the `atomvm:read_priv/2` function.

For this test, note the presence of the `favicon-32x32.png` in the `priv` directory.  This file will get embedded in the AVM file created as part of the build, making it available to the application via a call to the `atomvm:read_priv/2` function.  When this program is executed, you should see something like the following printed to the console (ellipsis added):

    <<-119,80,78,71,13,10,26,10,0,0,0,...>>

For more information about programming on the AtomVM platform, see the [AtomVM Programmers Guide](https://doc.atomvm.net/programmers-guide.html).

## Supported Platforms

| Platform | Supported |
|----------|-----------|
| `esp32`  | ✅ |
| `stm32`  | ✅ |
| `generic_unix`  | ✅ |

For general information about building and executing Erlang AtomVM example programs, see the Erlang example program [README](../README.md).
