#!/usr/bin/python3

import argparse
import numpy as np
from sklearn import neighbors

# Parse command line arguments.
parser = argparse.ArgumentParser(
    description='Run delta method with smaller number of different authors')
parser.add_argument(
    'trainfile',
    type=str,
    help='Data file location')
parser.add_argument(
    'testfile',
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

# Import data ([features...], truth, author).
traindata = np.loadtxt(args.trainfile, dtype=np.float)[:, 0:-2]
testdata = np.loadtxt(args.testfile, dtype=np.float)
y = testdata[:, -2].astype(np.bool)

feature_n = testdata.shape[1] - 2
feature_n_half = int(feature_n / 2)

X_known = testdata[:, 0:feature_n_half]
X_unknown = testdata[:, feature_n_half:-2]

traindata = np.vstack([traindata[:, 0:feature_n_half],
                       traindata[:, feature_n_half:]])

# Normalize the data.
if args.with_normalization:
    mean = np.mean(traindata, axis=0)
    std_var = np.std(traindata, axis=0)

    std_var[std_var == 0] = 1

    traindata = (traindata - mean) / std_var
    X_known = (X_known - mean) / std_var
    X_unknown = (X_unknown - mean) / std_var

# For each problem choose args.opposing_set_size number of different authors to
# test aginst.
problem_number = X_known.shape[0]
predictions = []
true_positives = 0
true_negatives = 0
false_negatives = 0
false_positives = 0
for i in range(0, problem_number):
    opposing = np.random.uniform(0, traindata.shape[0],
                                 args.opposing_set_size).astype(np.int)

    opposing_texts = traindata[opposing]
    opposing_classes = np.array([False] * args.opposing_set_size)

    knntrain = np.vstack([opposing_texts, X_known[i]])
    knnclass = np.hstack([opposing_classes, np.array(True)])

    model = neighbors.KNeighborsClassifier(
        n_neighbors=1, weights='uniform', algorithm='auto',
        metric='minkowski', p=args.metric)
    model.fit(knntrain, knnclass)

    prediction = model.predict(X_unknown[i].reshape(1, -1))[0]

    if prediction == y[i] and y[i]:
        true_positives += 1
    elif prediction == y[i] and not y[i]:
        true_negatives += 1
    elif prediction != y[i] and y[i]:
        false_negatives += 1
    else:
        false_positives += 1

    predictions.append(prediction)

predictions = np.array(predictions)
results = predictions == y

print(np.sum(results) / X_known.shape[0], true_positives,
      true_negatives, false_positives, false_negatives)
