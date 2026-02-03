#!/usr/bin/env python3
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

"""
Generate circuit diagrams using schemdraw for AtomVM tutorials.
"""

import schemdraw
import schemdraw.elements as elm
from schemdraw import pictorial
from pathlib import Path
import schemdraw.pictorial as pic

class Pico(elm.ElementImage):
    ''' Raspberry Pi Pico Element with proper pin positioning '''
    def __init__(self):
        # Get breadboard positions for scaling
        bb = pic.Breadboard()

        # Pico spans from row C to row H (left to right when USB is up)
        left_x = bb.anchors['B1'][0]  # Left side pins X position
        right_x = bb.anchors['I1'][0]  # Right side pins X position
        top_y = bb.anchors['B1'][1]
        bottom_y = bb.anchors['B21'][1]

        # Calculate width and height based on breadboard
        width = 2.9
        height = top_y - bottom_y

        # Position the image centered between C and H rows
        center_x = (left_x + right_x) / 2
        super().__init__('assets/raspberry-pi-pico.svg', width=width, height=height, xy=(center_x - width/2, -height+0.475))

        # Raspberry Pi Pico pinout (40 pins total, 20 per side)
        # Left side pins (1-20, bottom to top when USB is up)
        left_pins = [
            'GP0', 'GP1', 'GND', 'GP2', 'GP3', 'GP4', 'GP5', 'GND',
            'GP6', 'GP7', 'GP8', 'GP9', 'GND', 'GP10', 'GP11', 'GP12',
            'GP13', 'GND', 'GP14', 'GP15'
        ]

        # Right side pins (21-40, bottom to top when USB is up)
        right_pins = [
            'GP16', 'GP17', 'GND', 'GP18', 'GP19', 'GP20', 'GP21', 'GND',
            'GP22', 'RUN', 'GP26', 'GP27', 'GND', 'GP28', 'VBUS', 'VSYS',
            'GND', '3V3_EN', '3V3', 'GND'
        ]

        # Calculate pin spacing (20 pins over the height)
        pin_spacing = height / 21  # 21 intervals for 20 pins

        # Position left side pins (from bottom to top)
        for i, pin in enumerate(left_pins):
            y_pos = -height/2 + (i + 1) * pin_spacing
            self.anchors[pin] = (left_x, y_pos)

        # Position right side pins (from bottom to top)
        for i, pin in enumerate(right_pins):
            y_pos = -height/2 + (i + 1) * pin_spacing
            self.anchors[pin] = (right_x, y_pos)

def generate_led_breadboard():
    """Generate LED blinking breadboard circuit diagram."""
    elm.Line.defaults['lw'] = 4

    with schemdraw.Drawing(show = False) as d:
        # Add breadboard
        bb = pictorial.Breadboard()

        # Add Pico positioned on the breadboard
        pico = Pico()
        d += pico
        d += elm.Wire('c', k=-0.5).at(bb.B1).to(bb.B23).color('red').linewidth(4)
        d += pic.Resistor(150).at(bb.D23).to(bb.D26)
        d += pic.LED().at(bb.B27).up()
        d += elm.Wire('n', k=-1).at(bb.E27).to(bb.J18).linewidth(4)

    return d

def generate_led_circuit():
    """Generate LED blinking circuit diagram."""
    with schemdraw.Drawing(show = False) as d:
        # Microcontroller pin
        d += elm.Dot().label('GPIO', loc='right')

        # Resistor
        d += elm.Resistor().right().label('150 Ω')

        # LED
        d += elm.Diode().right().label('LED', loc='top')

        # Ground
        d += elm.Ground()

    return d

def main():
    """Generate all circuit diagrams."""
    output_dir = Path('build/assets/')
    output_dir.mkdir(exist_ok=True)

    led_drawing = generate_led_circuit()
    led_drawing.save(output_dir / 'led_circuit.svg')
    led_drawing = generate_led_breadboard()
    led_drawing.save(output_dir / 'led_breadboard.svg')

if __name__ == '__main__':
    main()
