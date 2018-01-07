#!/usr/bin/env python3

import argparse
import numpy as np
import pickle
from sklearn.ensemble import RandomForestClassifier

# Set up arguments
parser = argparse.ArgumentParser(
    description='Run Random Forest on extracted features')
parser.add_argument('file', type=str, help='Data File Location', nargs='+')
parser.add_argument(
    '--split', type=float, nargs='?',
    help='The percentage of the data used for training',
    default=0.5)
parser.add_argument(
    '--trees', type=int, nargs='?',
    help='The number of trees to use in random forest',
    default=10)

parser.add_argument(
    '--method', type=str, nargs='?',
    help='The encoding method used on the dataset',
    default='pancho')

parser.add_argument(
    '--importance', type=str, nargs='?',
    help='Determines if the feature feature_importance should \
                be printed as well, provide path to header file',
    default=None)

parser.add_argument(
    '--probability', type=str, nargs='?',
    help='Should the prediction probability be saved',
    default=None)

parser.add_argument(
    '--iterations', type=int, nargs='?',
    help='Help number of iterations to average over',
    default=100)
args = parser.parse_args()
datafiles = args.file


def encode(allText, known, unknown):
    return (np.power(known - unknown, 2) + 1) /\
        (np.power(allText - unknown, 2) + 1)


# Remove author number
X = np.loadtxt(datafiles[0], dtype=np.float)[:, :-1]
y = X[:, -1].astype(np.int)

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


# Run test 100 times.
feature_importance = []
probability = []
predictions = []
for i in range(args.iterations):
    # Shuffle such that we use random data in train and test.
    boundary = int(np.floor(len(X) * args.split))
    permutation = np.random.permutation(len(X))
    X = X[permutation]
    y = y[permutation]

    XTrain = X[:boundary]
    yTrain = y[:boundary]

    if args.split == 1.0:
        XTest = XTrain
        yTest = yTrain
    else:
        XTest = X[boundary:]
        yTest = y[boundary:]

    # Create model
    model = RandomForestClassifier(
        n_estimators=args.trees, n_jobs=-1)
    model.fit(XTrain, yTrain)

    if args.importance is not None:
        feature_importance.append(model.feature_importances_)

    if args.probability is not None:
        probability.append(model.predict_proba(XTest))

    p = model.predict(XTest) == yTest
    predictions.append(np.sum(p) / float(len(p)))

pickle.dump(model, open('forest.pickle', 'wb'))

print(np.mean(predictions))

if args.importance is not None:
    feature_importance = np.mean(feature_importance, 0)

    f = np.loadtxt('headers', dtype=str, delimiter='ø')
    feature_importance = zip(f, feature_importance)

    feature_importance = sorted(
        feature_importance, key=lambda x: x[1], reverse=True)

    with open(args.importance, 'w') as f:
        for i in feature_importance:
            f.write(str(i) + '\n')

if args.probability is not None:
    probability = np.mean(np.array(probability), 0)
    with open(args.probability, 'w') as f:
        for i in probability:
            f.write(str(i) + '\n')
