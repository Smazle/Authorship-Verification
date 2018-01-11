#!/usr/bin/python3

import matplotlib.pyplot as plt
import numpy as np

# Make pancho curve.
data = np.loadtxt('../data/ProbabilitiesPancho2015Test.data', dtype=np.float)
probabilities = data[:, 0]
results = data[:, 1].astype(np.bool)
inv_results = np.logical_not(results)

p_tprs = []
p_fprs = []
for threshold in np.arange(0.0, 1.0, 0.01):
    threshold_results = probabilities > threshold

    tp = sum(np.logical_and(threshold_results == results, results))
    fp = sum(np.logical_and(threshold_results != results, inv_results))
    tn = sum(np.logical_and(threshold_results == results, inv_results))
    fn = sum(np.logical_and(threshold_results != results, results))

    p_tprs.append(tp / float(tp + fn))
    p_fprs.append(fp / float(fp + tn))

p_tprs = np.array(p_tprs)
p_fprs = np.array(p_fprs)

p = p_fprs.argsort()
p_fprs = p_fprs[p]
p_tprs = p_tprs[p]

# Make minus curve.
data = np.loadtxt('../data/ProbabilitiesMinus2015Test.data', dtype=np.float)
probabilities = data[:, 0]
results = data[:, 1].astype(np.bool)
inv_results = np.logical_not(results)

m_tprs = []
m_fprs = []
for threshold in np.arange(0.0, 1.0, 0.01):
    threshold_results = probabilities > threshold

    tp = sum(np.logical_and(threshold_results == results, results))
    fp = sum(np.logical_and(threshold_results != results, inv_results))
    tn = sum(np.logical_and(threshold_results == results, inv_results))
    fn = sum(np.logical_and(threshold_results != results, results))

    m_tprs.append(tp / float(tp + fn))
    m_fprs.append(fp / float(fp + tn))

m_tprs = np.array(m_tprs)
m_fprs = np.array(m_fprs)

p = m_fprs.argsort()
m_fprs = m_fprs[p]
m_tprs = m_tprs[p]

# Plot graph.
plt.plot(p_fprs, p_tprs, marker='o', c='r', label='PANCHECO')
plt.plot(m_fprs, m_tprs, marker='o', c='b', label='MINUS')
plt.plot([0.0, 1.0], linestyle='--', c='tab:gray', label='Baseline x=x')
plt.grid(True)
plt.ylim(0.0, 1.0)
plt.xlim(0.0, 1.0)
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.legend()
plt.savefig('../pictures/forest_roc.png')
