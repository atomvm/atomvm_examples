<!---
  Copyright 2023 Fred Dushin <fred@dushin.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# `system_info` Application

Welcome to the `system_info` AtomVM application.

The `system_info` AtomVM application with collect and display various information about AtomVM and the environment in which it is running.

For example, on the ESP32 platform, this program will output the following information to the console:

    SystemInfo: #{atom_count => 169,port_count => 0,process_count => 1,system_architecture => <<"ESP_IDF-3.2-xtensa_esp32">>,word_size => 4}
    PlatformInfo: #{esp32_chip_info => {esp32,50,2,1},esp32_free_heap_size => 245816,esp32_largest_free_block => 116104,esp32_minimum_free_size => 242136,esp_idf_version => "v3.3.4-dirty"}
    ProcessInfo: [{<0.1.0>,[{heap_size,457},{stack_size,12},{message_queue_len,0},{memory,512}]}]

For more information about programming on the AtomVM platform, see the [AtomVM Programmers Guide](https://www.atomvm.net/doc/master/programmers-guide.html).

## Supported Platforms

For general information about building and executing Erlang AtomVM example programs, see the Erlang example program [README](../README.md).
