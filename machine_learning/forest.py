#!/usr/bin/python3

import argparse
import numpy as np
from sklearn.ensemble import RandomForestClassifier

# Set up arguments
parser = argparse.ArgumentParser(
    description='Run Random Forest on extracted features')

parser.add_argument('file', type=str, help='Data File Location', nargs='+')
parser.add_argument(
    '--split', type=float, nargs='?',
    help='The percentage of the data used for training')

parser.add_argument(
    '--method', type=str, nargs='?',
    help='The encoding method used on the dataset')

args = parser.parse_args()
datafiles = args.file

if args.split is None:
    args.split = .5
if args.method is None:
    args.method = 'Normal'


def encode(allText, known, unknown):
    return (np.power(known - unknown, 2) + 1) /\
        (np.power(allText - unknown, 2) + 1)


# Remove author number
X = np.loadtxt(datafiles[0], dtype=np.float)[:, :-1]
y = [int(i) for i in X[:, -1]]

feature_n = X.shape[1] - 1
feature_n_half = int(feature_n / 2)

X_known = X[:, 0:feature_n_half]
X_unknown = X[:, feature_n_half:-1]

X = np.column_stack((X_known, X_unknown))


# If two files are supplied (One containing the total features against itself)
# then encode it, as per pancho.
if len(datafiles) > 1:
    if args.method == 'pancho':
        allText = np.loadtxt(datafiles[1], dtype=np.float)
        newX = []
        for known, unknown in zip(X_known, X_unknown):
            newX.append(encode(allText, known, unknown))

        X = np.array(newX)
    else:
        if args.method == 'minus':
            newX = []
            for known, unknown in zip(X_known, X_unknown):
                newX.append(known - unknown)

            X = np.array(newX)


# Get training and test set split - Randomly
predictions = []
for _ in range(10):
    boundary = int(np.floor(len(X) * args.split))
    np.random.shuffle(X)

    XTrain = X[:boundary]
    yTrain = y[:boundary]

    if(args.split == 1.0):
        XTest = XTrain
        yTest = yTrain
    else:
        XTest = X[boundary:]
        yTest = y[boundary:]

    # Create model
    model = RandomForestClassifier(n_estimators=10, n_jobs=-1)
    model.fit(XTrain, yTrain)

    p = model.predict(XTest) == yTest
    predictions.append(np.sum(p) / float(len(p)))

print(np.mean(predictions))
