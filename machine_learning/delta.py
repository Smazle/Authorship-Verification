#!/usr/bin/python3

import argparse
import numpy as np
from sklearn import neighbors

# Parse command line arguments.
parser = argparse.ArgumentParser(
    description='Run delta method with smaller number of different authors')
parser.add_argument(
    'file',
    type=str,
    help='Data file location')
parser.add_argument(
    '--with-normalization',
    help='Whether or not to normalize data.',
    action='store_true',
    default=False)
parser.add_argument(
    '--opposing-set-size',
    help='Number of opposing authors to use',
    type=int,
    default=5)
parser.add_argument(
    '--metric',
    help='Which Minkowski metric to use given 1 it will be the Manhattan ' +
        'distance and 2 it is the Euclidean distance',
    type=int,
    default=1)
args = parser.parse_args()

X = np.loadtxt(args.file, dtype=np.float)
y = X[:, -1].astype(np.bool)

feature_n = X.shape[1] - 1
feature_n_half = int(feature_n / 2)

X_known = X[:, 0:feature_n_half]
X_unknown = X[:, feature_n_half:-1]

# Normalize the data.
if args.with_normalization:
    mean = np.mean(np.vstack([X_known, X_unknown]), axis=0)
    std_var = np.std(np.vstack([X_known, X_unknown]), axis=0)

    X_known = (X_known - mean) / std_var
    X_unknown = (X_unknown - mean) / std_var

# For each author choose args.opposing_set_size number of different authors to
# test aginst.
author_number = X_known.shape[0]
predictions = []
for i in range(0, author_number):
    opposing = np.random.uniform(0, author_number, args.opposing_set_size)\
        .astype(np.int)

    known = X_known[np.append(opposing, i)]
    unknown = np.reshape(X_unknown[i], (1, X_known.shape[1]))

    model = neighbors.KNeighborsClassifier(
        n_neighbors=1,
        weights='uniform',
        algorithm='auto',
        metric='minkowski',
        p=args.metric)
    model.fit(known, np.append(opposing, i))

    predictions.append(model.predict(unknown)[0])

predictions = np.array(predictions)
results = predictions == np.array(range(0, 100))
print(np.sum(results == y))
