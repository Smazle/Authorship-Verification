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
parser.add_argument(
    '--with-PN',
    help='Whether or not to also print True Positive, False Positive, True ' +
    'Negative and False Negative.',
    action='store_true',
    default=False)
args = parser.parse_args()

# Import data ([features...], truth, author).
data = np.loadtxt(args.file, dtype=np.float)
y = data[:, -2].astype(np.bool)
authors = data[:, -1].astype(np.int)

feature_n = data.shape[1] - 2
feature_n_half = int(feature_n / 2)

X_known = data[:, 0:feature_n_half]
X_unknown = data[:, feature_n_half:-2]

# Normalize the data.
if args.with_normalization:
    mean = np.mean(np.vstack([X_known, X_unknown]), axis=0)
    std_var = np.std(np.vstack([X_known, X_unknown]), axis=0)

    std_var[std_var == 0] = 1

    X_known = (X_known - mean) / std_var
    X_unknown = (X_unknown - mean) / std_var

# For each problem choose args.opposing_set_size number of different authors to
# test aginst.
problem_number = X_known.shape[0]
predictions = []
true_positives = 0
true_negatives = 0
false_positives = 0
false_negatives = 0
for i in range(0, problem_number):
    opposing = np.random.uniform(0, problem_number, args.opposing_set_size)\
        .astype(np.int)

    known = X_known[np.append(opposing, i)]
    unknown = np.reshape(X_unknown[i], (1, X_known.shape[1]))

    results = authors[np.append(opposing, i)]

    model = neighbors.KNeighborsClassifier(
        n_neighbors=1, weights='uniform', algorithm='auto', metric='minkowski',
        p=args.metric)
    model.fit(known, results)

    prediction = model.predict(unknown)[0]

    if prediction == authors[i] and y[i]:
        true_positives = true_positives + 1
    elif prediction == authors[i] and not y[i]:
        false_positives = false_positives + 1
    elif prediction != authors[i] and y[i]:
        false_negatives = false_negatives + 1
    else:
        true_negatives = true_negatives + 1

    predictions.append(prediction)

predictions = np.array(predictions)
results = predictions == authors

if args.with_PN:
    print(np.sum(results == y) / X_known.shape[0], true_positives,
          true_negatives, false_positives, false_negatives)
else:
    print(np.sum(results == y) / X_known.shape[0])
