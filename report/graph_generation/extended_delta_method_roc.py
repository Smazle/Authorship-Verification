#!/usr/bin/python3

import matplotlib.pyplot as plt
import numpy as np

# 13 1.
tprs = np.array([
    1.0, 0.652, 0.5064, 0.405, 0.3472, 0.3018, 0.26039999999999996, 0.2334,
    0.2168, 0.2006, 0.18280000000000002])
fprs = np.array([
    1.0, 0.44780000000000003, 0.2626, 0.1636, 0.1304, 0.1048,
    7.919999999999999e-2, 6.62e-2, 5.7999999999999996e-2, 5.04e-2,
    4.6799999999999994e-2])

p = fprs.argsort()
fprs = fprs[p]
tprs = tprs[p]

plt.plot(fprs, tprs, marker='o', c='r', label='PAN2015 Test Dataset')
plt.plot([0.0, 1.0], linestyle='--', c='tab:gray', label='Baseline x=x')
plt.grid(True)
plt.ylim(0.0, 1.0)
plt.xlim(0.0, 1.0)
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.legend()
plt.savefig('../pictures/extended_delta_method_roc.png')
