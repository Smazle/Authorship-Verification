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
predictions = []
for i in range(100):
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
        n_estimators=args.trees, n_jobs=-1, max_features=None)
    model.fit(XTrain, yTrain)
    feature_importance.append(model.feature_importances_)

    p = model.predict(XTest) == yTest
    predictions.append(np.sum(p) / float(len(p)))

print(np.mean(predictions))

feature_importance = np.mean(feature_importance, 0)

f = open('../feature_extraction/headers', 'r').read().split(' ')
feature_importance = zip(f, feature_importance)

feature_importance = sorted(
    feature_importance, key=lambda x: x[1], reverse=True)

for i in feature_importance:
    print(i)
