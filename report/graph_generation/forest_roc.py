#!/usr/bin/python3

import matplotlib.pyplot as plt
import numpy as np

data = np.loadtxt('../data/ProbabilitiesPancho2015Test.data', dtype=np.float)
probabilities = data[:, 0]
results = data[:, 1].astype(np.bool)
inv_results = np.logical_not(results)

# print(results == (probabilities > 0.5))

tprs = []
fprs = []
for threshold in np.arange(0.0, 1.0, 0.01):
    threshold_results = probabilities > threshold

    tp = sum(np.logical_and(threshold_results == results, results))
    fp = sum(np.logical_and(threshold_results != results, inv_results))
    tn = sum(np.logical_and(threshold_results == results, inv_results))
    fn = sum(np.logical_and(threshold_results != results, results))

    tprs.append(tp / float(tp + fn))
    fprs.append(fp / float(fp + tn))

tprs = np.array(tprs)
fprs = np.array(fprs)

p = fprs.argsort()
fprs = fprs[p]
tprs = tprs[p]

plt.plot(fprs, tprs, marker='o', c='r', label='PANCHO')
plt.plot([0.0, 1.0], linestyle='--', c='tab:gray', label='Baseline x=x')
plt.grid(True)
plt.ylim(0.0, 1.0)
plt.xlim(0.0, 1.0)
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.legend()
plt.show()
