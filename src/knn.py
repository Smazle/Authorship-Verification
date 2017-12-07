#!/usr/bin/python3

import sys
import numpy as np
from sklearn import neighbors
import argparse

# Parse command line arguments.
parser = argparse.ArgumentParser(description='Run KNN on extracted features')
parser.add_argument('file', type=str, help='Data file location')
parser.add_argument('--with-normalization',
        help='Whether or not to normalize data.', action='store_true')
args = parser.parse_args()

datafile = args.file

X = np.loadtxt(datafile, dtype=np.float)
y = X[:,-1].astype(np.bool)

feature_n = X.shape[1] - 1
feature_n_half = int(feature_n / 2)

X_known = X[:,0:feature_n_half]
X_unknown = X[:,feature_n_half:-1]

# Normalize the data.
if args.with_normalization:
    mean = np.mean(np.vstack([X_known, X_unknown]), axis=0)
    std_var = np.std(np.vstack([X_known, X_unknown]), axis=0)

    X_known = (X_known - mean) / std_var
    X_unknown = (X_unknown - mean) / std_var

# With Manhattan distance.
model = neighbors.KNeighborsClassifier(n_neighbors=1, weights='uniform',
        algorithm='auto', metric='minkowski', p=1)
model.fit(X_known, np.array(range(1, 101)))

predictions = model.predict(X_unknown) == np.array(range(1, 101))
print('correct Manhattan', np.sum(predictions==y))

# With Euclidean distance.
model = neighbors.KNeighborsClassifier(n_neighbors=1, weights='uniform',
        algorithm='auto', metric='minkowski', p=2)
model.fit(X_known, np.array(range(1, 101)))

predictions = model.predict(X_unknown) == np.array(range(1, 101))
print('correct Euclidean', np.sum(predictions==y))
