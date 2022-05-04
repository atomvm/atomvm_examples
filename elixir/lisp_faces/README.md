<!---
  Copyright 2022 Davide Bettio <davide@uninstall.it>

  SPDX-License-Identifier: Apache-2.0
-->

# LISP Faces

This example turns your M5Stack Faces into a pocket LISP computer using AtomVM and its internal
LISP interpreter.

This example has been tested with OTP 21, but any OTP version previous to 24 should work.

In order to get it working you need to build and flash AtomVM with an additional display component
for the [ILI934x display](https://github.com/atomvm/avm_ili934x).

## Build

avm_deps directory should be created, and it should be populated with any library that is not
already installed in lib partition.

```
mkdir avm_deps`
```

In order to build the application AVM file (`lisp_faces.avm`) just run:

```
mix atomvm.packbeam
```

## Flash

Just run:
```
mix atomvm.esp32.flash
```
