import numpy as np
import matplotlib.pyplot as plt
from numpy.polynomial.polynomial import Polynomial

# 1:1 - 1.00 - 500x500
# 5:4 - 1.25 - 640x512
# 4:3 - 1.33 - 640x480
# 3:2 - 1.50 - 480x320
# 16:10 - 1.60 - 640x400
# 16:9 - 1.78 - 640x360
# 1.85:1 - 1.85 - 640x346
# 21:9 - 2.33 - 640x274
# 2.35:1 - 2.35 - 640x272
# 2.39:1 - 2.39 - 640x267
# 32:9 - 3.56 - 1280x360

# (ml "progress-pc")
# (pc-set-window-size 500 500)
# (set! (-> *progress-process* 0 left-x-offset) 20)
# (set! (-> *progress-process* 0 right-x-offset) 20)
# (set! *PC-CROSS-X-ADJUST* -20.0)
# (set! *PC-CROSS-Y-ADJUST* -20.0)
# (set! *PC-SQUARE-X-ADJUST* -20.0)
# (set! *PC-SQUARE-Y-ADJUST* -20.0)

values = [
    {
        'aspect': 1.00,
        'left': 20,
        'right': -33,
        'cross-x': -20.0,
        'cross-y': -20.0,
        'square-x': -5.0,
        'square-y': -10.0,
        'triangle-x': 0.0,
        'triangle-y': 0.0,
        'circle-x': 0.0,
        'circle-y': 0.0,
        'percent-x': 0.0,
        'autosave-x': 0.0,
        'orb-x': 0.0,
        'orb-glow-x': 0.0,
        'orb-text-x': 0.0,
        'cell-x': 0.0,
        'cell-text-x': 0.0,
        'buzzer-text-x': 0.0,
        'options-text-x': 0.0
    },
    {
        'aspect': 1.25,
        'left': 4,
        'right': -6,
        'cross-x': -5.0,
        'cross-y': 0.0,
        'square-x': 0.0,
        'square-y': 0.0,
        'triangle-x': 0.0,
        'triangle-y': 0.0,
        'circle-x': 0.0,
        'circle-y': 0.0,
        'percent-x': 0.0,
        'autosave-x': 0.0,
        'orb-x': 0.0,
        'orb-glow-x': 0.0,
        'orb-text-x': 0.0,
        'cell-x': 0.0,
        'cell-text-x': 0.0,
        'buzzer-text-x': 0.0,
        'options-text-x': 0.0
    },
    {
        'aspect': 1.33,
        'left': 0,
        'right': 0,
        'cross-x': 0.0,
        'cross-y': 0.0,
        'square-x': 0.0,
        'square-y': 0.0,
        'triangle-x': 0.0,
        'triangle-y': 0.0,
        'circle-x': 0.0,
        'circle-y': 0.0,
        'percent-x': 0.0,
        'autosave-x': 0.0,
        'orb-x': 0.0,
        'orb-glow-x': 0.0,
        'orb-text-x': 0.0,
        'cell-x': 0.0,
        'cell-text-x': 0.0,
        'buzzer-text-x': 0.0,
        'options-text-x': 0.0
    },
    {
        'aspect': 1.5,
        'left': -6,
        'right': 12,
        'cross-x': 5.0,
        'cross-y': 0.0,
        'square-x': 5.0,
        'square-y': 0.0,
        'triangle-x': 0.0,
        'triangle-y': 0.0,
        'circle-x': 0.0,
        'circle-y': 0.0,
        'percent-x': 12.0,
        'autosave-x': 10.0,
        'orb-x': 0.0,
        'orb-glow-x': 0.0,
        'orb-text-x': 0.0,
        'cell-x': 0.0,
        'cell-text-x': 0.0,
        'buzzer-text-x': 0.0,
        'options-text-x': 0.0
    },
    {
        'aspect': 1.60,
        'left': -10,
        'right': 18,
        'cross-x': 8.0,
        'cross-y': 5.0,
        'square-x': 5.0,
        'square-y': 0.0,
        'triangle-x': 0.0,
        'triangle-y': 0.0,
        'circle-x': 0.0,
        'circle-y': 0.0,
        'percent-x': 12.0,
        'autosave-x': 10.0,
        'orb-x': 0.0,
        'orb-glow-x': 0.0,
        'orb-text-x': 0.0,
        'cell-x': 0.0,
        'cell-text-x': 0.0,
        'buzzer-text-x': 0.0,
        'options-text-x': 0.0
    },
    {
        'aspect': 1.78,
        'left': -14,
        'right': 26,
        'cross-x': 15.0,
        'cross-y': 10.0,
        'square-x': 8.0,
        'square-y': 10.0,
        'triangle-x': 0.0,
        'triangle-y': 0.0,
        'circle-x': 0.0,
        'circle-y': 0.0,
        'percent-x': 15.0,
        'autosave-x': 10.0,
        'orb-x': 0.0,
        'orb-glow-x': 0.0,
        'orb-text-x': 0.0,
        'cell-x': 0.0,
        'cell-text-x': 0.0,
        'buzzer-text-x': 0.0,
        'options-text-x': 0.0
    },
    {
        'aspect': 1.85,
        'left': -16,
        'right': 29,
        'cross-x': 17.0,
        'cross-y': 10.0,
        'square-x': 8.0,
        'square-y': 10.0,
        'triangle-x': 0.0,
        'triangle-y': 0.0,
        'circle-x': 0.0,
        'circle-y': 0.0,
        'percent-x': 15.0,
        'autosave-x': 10.0,
        'orb-x': 0.0,
        'orb-glow-x': 0.0,
        'orb-text-x': 0.0,
        'cell-x': 0.0,
        'cell-text-x': 0.0,
        'buzzer-text-x': 0.0,
        'options-text-x': 0.0
    },
    {
        'aspect': 2.33,
        'left': -25,
        'right': 45,
        'cross-x': 30.0,
        'cross-y': 20.0,
        'square-x': 15.0,
        'square-y': 15.0,
        'triangle-x': 2.0,
        'triangle-y': 5.0,
        'circle-x': 2.0,
        'circle-y': -7.0,
        'percent-x': 18.0,
        'autosave-x': 15.0,
        'orb-x': 0.0,
        'orb-glow-x': 0.0,
        'orb-text-x': 0.0,
        'cell-x': 0.0,
        'cell-text-x': 0.0,
        'buzzer-text-x': 0.0,
        'options-text-x': 0.0
    },
    {
        'aspect': 2.35,
        'left': -26,
        'right': 46,
        'cross-x': 30.0,
        'cross-y': 20.0,
        'square-x': 15.0,
        'square-y': 15.0,
        'triangle-x': 2.0,
        'triangle-y': 5.0,
        'circle-x': 2.0,
        'circle-y': -7.0,
        'percent-x': 18.0,
        'autosave-x': 15.0,
        'orb-x': 0.0,
        'orb-glow-x': 0.0,
        'orb-text-x': 0.0,
        'cell-x': 0.0,
        'cell-text-x': 0.0,
        'buzzer-text-x': 0.0,
        'options-text-x': 0.0
    },
    {
        'aspect': 3.56,
        'left': -37,
        'right': 65,
        'cross-x': 45.0,
        'cross-y': 25.0,
        'square-x': 17.0,
        'square-y': 15.0,
        'triangle-x': 3.0,
        'triangle-y': 5.0,
        'circle-x': 5.0,
        'circle-y': -10.0,
        'percent-x': 25.0,
        'autosave-x': 18.0,
        'orb-x': 5.0,
        'orb-glow-x': 4.0,
        'orb-text-x': 10.0,
        'cell-x': -10.0,
        'cell-text-x': 2.0,
        'buzzer-text-x': -5.0,
        'options-text-x': 3.0
    },
]

adjustments = [
    'left', 'right', 'cross-x', 'cross-y', 'square-x', 'square-y', 'triangle-x', 'triangle-y', 'circle-x', 'circle-y', 'percent-x', 'autosave-x', 'orb-x', 'orb-glow-x', 'orb-text-x', 'cell-x', 'cell-text-x', 'buzzer-text-x', 'options-text-x'
]

aspect_ratio_values = []
for dataset in values:
    aspect_ratio_values.append(dataset['aspect'])
aspect_ratios = np.array(aspect_ratio_values)

def check_return_value(aspect_ratio, coefs):
    return coefs[0] + coefs[1] * aspect_ratio + coefs[2] * aspect_ratio**2

for adjust in adjustments:
    # collect the values
    data_values = []
    for dataset in values:
        data_values.append(dataset[adjust])
    np_values = np.array(data_values)
    # polynomial regression to interpolate between values
    poly = Polynomial.fit(aspect_ratios, np_values, 3)
    coefficients = poly.convert().coef
    # produce a goal function to represent this polynomial
    # if the aspect-ratio is close within a threshold we use that
    function_string = ""
    function_string += f"(defun pc-sprite-adjust-{adjust} ((aspect-ratio float))\n"
    function_string += f"  (cond\n"
    for aspect_ratio, value in zip(aspect_ratios, data_values):
        function_string += f"   ((fequal-epsilon? aspect-ratio {aspect_ratio} 0.01) {value:.1f})\n"
    function_string += f"   (else\n    (+ {coefficients[0]}\n     (* {coefficients[1]} aspect-ratio)\n     (* {coefficients[2]} aspect-ratio aspect-ratio)\n     (* {coefficients[3]} aspect-ratio aspect-ratio aspect-ratio)))))\n"
    print(function_string)
