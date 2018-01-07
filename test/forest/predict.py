#!/usr/bin/env python3

import numpy as np
import pickle
import sys


datafiles = sys.argv[1:]

model = pickle.load(open('forest.pickle', 'rb'))


def encode(allText, known, unknown):
    return (np.power(known - unknown, 2) + 1) /\
        (np.power(allText - unknown, 2) + 1)


X = np.loadtxt(datafiles[0], dtype=np.float)[:, :-1]
y = X[:, -1].astype(np.int)

feature_n = X.shape[1] - 1
feature_n_half = int(feature_n / 2)
X_known = X[:, 0:feature_n_half]
X_unknown = X[:, feature_n_half:-1]

print(X.shape)
print(y.shape)

newX = []

if len(datafiles) > 1:
    master = np.loadtxt(datafiles[1], dtype=np.float)
    print('Pancho Encoding')
    for known, unknown in zip(X_known, X_unknown):
        newX.append(encode(master, known, unknown))
else:
    print('Minus Encoding')
    for known, unknown in zip(X_known, X_unknown):
        newX.append(known - unknown)

X = np.array(newX)

print(X.shape)
print(y.shape)

res = model.predict(X) == y
print('Precision: ', np.sum(res) / float(len(res)))

np.savetxt('Probabilities.data', model.predict_proba(X))
print('Probabilities output to Probabilities.data')
