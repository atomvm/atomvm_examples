# `morse_server` Application

Welcome to the `morse_server` AtomVM application.

The `morse_server` AtomVM application uses the `http_server` module and demonstrates retrieving and using variables from http input forms, and uses Erlang list recursion to process input text and convert it to morse symbols.

## Usage

> **IMPORTANT** before you compile and flash this example you need to edit `myssid` and `mypsk` in src/morse_server.erl to match your wireless network configuration.
```
    Config = [
       {sta, [
            {ssid, esp:nvs_get_binary(atomvm, sta_ssid, <<"myssid">>)},
            {psk,  esp:nvs_get_binary(atomvm, sta_psk, <<"mypsk">>)},
```

The `morse_server` will accept input text and flash it out in morse code on the specified LED.  The default on-board LED for most ESP32 boards is GPIO 2.  You may also connect an LED to any GPIO pin and connect its anode to ground with a 100 Ohm or higher (220 Ohm recommended) resistor.

When the ESP32 first boots up, at at the end of the serial output, you should see output similar to:

    I (621) NETWORK: DHCP hostname set to atomvm-240ac458d278
    I (741) wifi: n:1 1, o:1 0, ap:255 255, sta:1 1, prof:1
    I (1721) wifi: state: init -> auth (b0)
    I (1731) wifi: state: auth -> assoc (0)
    I (1731) wifi: state: assoc -> run (10)
    I (1761) wifi: connected with NETGEAR_EXT, channel 1
    I (1761) wifi: pm start, type: 1

    I (1771) NETWORK: SYSTEM_EVENT_STA_CONNECTED received.
    connected
    I (2821) event: sta ip: 192.168.0.32, mask: 255.255.255.0, gw: 192.168.0.1
    I (2821) NETWORK: SYSTEM_EVENT_STA_GOT_IP: 192.168.0.32

Your ESP can be reached with a web browser on port 8080 by its IP address or DHCP hostname, in the example above this would either be:

    "http://192.168.0.32:8080"  or  "http://atomvm-240ac458d278:8080"

For more information about programming on the AtomVM platform, see the [AtomVM Programmers Guide](https://doc.atomvm.net/programmers-guide.html).

## Supported Platforms

| Platform       | Supported |
|----------------|-----------|
| `esp32`        |    ✅     |
| `stm32`        |    ❌     |
| `generic_unix` |    ❌     |

For general information about building and executing Erlang AtomVM example programs, see the Erlang example program [README](../README.md).




