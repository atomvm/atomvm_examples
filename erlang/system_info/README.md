# `system_info` Application

Welcome to the `system_info` AtomVM application.

The `system_info` AtomVM application with collect and display various information about AtomVM and the environment in which it is running.

For example, on the ESP32 platform, this program will output the following information to the console:

    SystemInfo: #{atom_count => 169,port_count => 0,process_count => 1,system_architecture => <<"ESP_IDF-3.2-xtensa_esp32">>,word_size => 4}
    PlatformInfo: #{esp32_chip_info => {esp32,50,2,1},esp32_free_heap_size => 245816,esp32_largest_free_block => 116104,esp32_minimum_free_size => 242136,esp_idf_version => "v3.3.4-dirty"}
    ProcessInfo: [{<0.1.0>,[{heap_size,457},{stack_size,12},{message_queue_len,0},{memory,512}]}]

For more information about programming on the AtomVM platform, the [AtomVM Programmers Guide](https://doc.atomvm.net/programmers-guide.html).

For general information about building and executing AtomVM example programs, see the [Example Program README](../README.md).

## Building

To build and package this application into an AtomVM AVM file, use the `packbeam` target:

    shell$ rebar3 packbeam

This target will create the `system_info.avm` file in the `./_build/default/lib/` directory.

## Running on the ESP32 platform

To run this application on the ESP32 platform, you must flash the application to the device attached to your computer via USB.  You may then optionally monitor the program via a serial console program to view any data output to the console.

### Flashing onto an ESP32 Device

To flash this application to your ESP32 device, issue the `esp32_flash` target.  Use the `--port` option to specify the port to which your device is connected:

    shell$ rebar3 esp32_flash --port /dev/ttyUSB0

### Monitoring an ESP32 Device

Use a serial console program, such as `minicom`, to attach to the device over USB:

    shell$ minicom -D /dev/ttyUSB0

## Running on Generic Unix platforms

To run this application on a generic UNIX platform, supply the path to the generated AVM file, together with the AtomVM core library

    shell% /path/to/AtomVM ./_build/default/lib/system_info.avm /path/to/atomvmlib.avm

> Note.  Currently, you must build the AtomVM virtual machine from source, in order to run AtomVM applications on the generic UNIX platform.
