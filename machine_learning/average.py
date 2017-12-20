#!/usr/bin/python3

import argparse
import numpy as np
from scipy.spatial.distance import cityblock


parser = argparse.ArgumentParser(
    description='Run Random Forest on extracted features')

parser.add_argument('file', type=str, help='Data File Location', nargs='?')
args = parser.parse_args()


# Remove author number
X = np.loadtxt(args.file, dtype=np.float)[:, :-1]
y = [int(i) for i in X[:, -1]]

feature_n = X.shape[1] - 1
feature_n_half = int(feature_n / 2)

X_known = X[:, 0:feature_n_half]
X_unknown = X[:, feature_n_half:-1]

avg_X = np.mean(X_known, 1)

res = np.zeros(len(X))

for i in range(len(X_unknown)):
    # if np.linalg.norm(X_unknown[:,i] - avg_X) < \
    #        np.linalg.norm(X_unknown[:,i] - X_known[:, i]):
    #    res[i] = 1
    if cityblock(X_unknown[:, 1], avg_X) < \
            cityblock(X_unknown[:, i], X_known[:, i]):
        res[i] = 1

print(res)
print(np.sum(res == y))
