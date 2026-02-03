#
# This file is part of AtomVM.
#
# Copyright 2025 Paul Guyot <pguyot@kallisys.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

# Configuration file for the Sphinx documentation builder.


# -- Project information -----------------------------------------------------
project = 'AtomVM Tutorials'
copyright = '2025, AtomVM contributors'
author = 'AtomVM'

# -- General configuration ---------------------------------------------------
extensions = [
    'myst_parser',
    'sphinx_rtd_theme',
    'sphinx.ext.autodoc',
    'sphinx.ext.viewcode',
    'sphinx.ext.todo',
    'sphinx_inline_tabs',
]

# MyST settings
myst_enable_extensions = [
    "colon_fence",
    "deflist",
    "dollarmath",
    "fieldlist",
    "html_admonition",
    "html_image",
    "replacements",
    "smartquotes",
    "strikethrough",
    "substitution",
    "tasklist",
]

myst_heading_anchors = 3

# -- Options for HTML output -------------------------------------------------
html_theme = 'sphinx_rtd_theme'
html_static_path = ['assets']
html_meta = {
    "description lang=en": "AtomVM tutorials",
    "keywords": "AtomVM, Erlang, Elixir, Gleam, BEAM, IoT, embedded, ESP32, STM32, RP2040, Raspberry Pi Pico, WASM, libAtomVM, eavmlib, packbeam",
    "property=og:locale": "en_US"
}

# -- Options for EPUB output --------------------------------------------------
epub_show_urls = 'footnote'

exclude_patterns = [
    'tmp',
    'venv',
]
