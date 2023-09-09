# `supervised_discs` Application

Welcome to the `supervised_discs` AtomVM application.

The `supervised_discs` demonstrates OTP supervisors within a simple HTML5 game.

## Usage

You need add the following files to the `html/bin/` directory:
- AtomVM.js
- AtomVM.wasm
- AtomVM.worker.js
- atomvmlib.avm
- supervised_discs.avm

The first three are the AtomVM virtual machine compiled for emscripten platform.
`atomvmlib.avm` is the AtomVM library
`supervised_discs.avm` is built with rebar in this directory.

Then you need to put the HTML directory in an HTTPS server that serve the
following headers:
  Cross-Origin-Opener-Policy: same-origin
  Cross-Origin-Embedder-Policy: require-corp

You can push the whole html directory to netlify as the `_headers` file already
has the proper content.
