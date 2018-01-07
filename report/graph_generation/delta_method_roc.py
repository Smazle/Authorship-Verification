#!/usr/bin/python3

import matplotlib.pyplot as plt
import numpy as np

# 13 1.
tprs13_1 = np.array([
    1.0, 0.7070000000000001, 0.60625, 0.55675, 0.5175, 0.49225, 0.47375,
    0.46275000000000005, 0.44575, 0.4405, 0.42675])
fprs13_1 = np.array([
    1.0, 0.5112244897959184, 0.36448979591836733, 0.3038775510204082,
    0.2646938775510204, 0.24877551020408162, 0.22653061224489796,
    0.20918367346938777, 0.19918367346938776, 0.18918367346938775,
    0.1816326530612245])

p = fprs13_1.argsort()
fprs13_1 = fprs13_1[p]
tprs13_1 = tprs13_1[p]

# 13 2.
tprs13_2 = np.array([
    1.0, 0.7187301587301588, 0.6192063492063492, 0.5593650793650794,
    0.5377777777777778, 0.5123809523809524, 0.5, 0.4873015873015873,
    0.4795238095238095, 0.47269841269841273, 0.4612698412698413])
fprs13_2 = np.array([
    1.0, 0.5509375, 0.41640625, 0.3446875, 0.295625, 0.27609375, 0.24625,
    0.22578125, 0.2121875, 0.21171875, 0.198125])

p = fprs13_2.argsort()
fprs13_2 = fprs13_2[p]
tprs13_2 = tprs13_2[p]

# 15.
tprs15 = np.array([
    1.0, 0.5134000000000001, 0.3518, 0.2316, 0.1822, 0.1398, 0.1176,
    9.480000000000001e-2, 8.0e-2, 6.96e-2, 5.84e-2])
fprs15 = np.array([
    1.0, 0.396, 0.2208, 0.1522, 0.1246, 9.44e-2, 7.6e-2, 6.68e-2, 5.46e-2,
    4.32e-2, 4.16e-2])

p = fprs15.argsort()
fprs15 = fprs15[p]
tprs15 = tprs15[p]

plt.plot(fprs13_1, tprs13_1, marker='o', c='r', label='PAN2013 Test Dataset 1')
plt.plot(fprs13_2, tprs13_2, marker='o', c='b', label='PAN2013 Test Dataset 2')
plt.plot(fprs15, tprs15, marker='o', c='g', label='PAN2015 Test Dataset')
plt.plot([0.0, 1.0], linestyle='--', c='tab:gray', label='Baseline x=x')
plt.grid(True)
plt.ylim(0.0, 1.0)
plt.xlim(0.0, 1.0)
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.legend()
plt.savefig('../pictures/delta_method_roc.png')
