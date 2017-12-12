#!/usr/bin/python3

import sys
import numpy as np
import argparse
from sklearn.ensemble import RandomForestClassifier

parser = argparse.ArgumentParser(description=
        "Run Random Forest on extracted features")

parser.add_argument('file', type=str, help='Data File Location')

args = parser.parse_args()
datafile = args.file

X = np.loadtxt(datafile, dtype=np.float)
y = X[:,-1]

feature_n = X.shape[1] - 1
feature_n_half = int(feature_n / 2)

X_known = X[:,0:feature_n_half]
X_unknown = X[:,feature_n_half:-1]

authors = np.array(range(1,101))


model = RandomForestClassifier()
model.fit(X_known, authors)

predictions = model.predict(X_unknown) == np.array(range(1, 101))
print("Correct Random Forest", np.sum(predictions==y))


