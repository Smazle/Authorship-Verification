#!/usr/bin/python3

import argparse
import numpy as np
from numpy.random import choice
from sklearn.svm import SVC
from sklearn.model_selection import LeaveOneOut
from sklearn.model_selection import GridSearchCV


# Set up arguments
parser = argparse.ArgumentParser(
    description="Run author specific SVM classifier on input data given.")

parser.add_argument('training_file', type=str, help='Data File Location')
parser.add_argument('test_file', type=str, help='Data File Location')

parser.add_argument(
    '--with-normalization',
    help='Whether or not to normalize data.',
    action='store_true',
    default=False)

parser.add_argument(
    '--c',
    help='Hyperparameter for SVM',
    type=float)

parser.add_argument(
    '--gamma',
    help='Hyperparamter for SVM',
    type=float)

args = parser.parse_args()

# Load training data and remove results and author number.
traindata = np.loadtxt(args.training_file, dtype=np.float)
traindata = traindata[:, 0:-2]
half = traindata.shape[1] // 2
traindata = np.vstack([traindata[:, :half], traindata[:, half:]])

# Load test data.
testdata = np.loadtxt(args.test_file, dtype=np.float)
authors = testdata[:, -1].astype(np.int)
results = testdata[:, -2].astype(np.bool)
half = (testdata.shape[1] - 2) // 2
X_known = testdata[:, 0:half]
X_unknown = testdata[:, half:-2]

# Normalize the data.
if args.with_normalization:
    mean = np.mean(traindata, axis=0)
    std_var = np.std(traindata, axis=0)

    std_var[std_var == 0] = 1

    traindata = (traindata - mean) / std_var
    X_known = (X_known - mean) / std_var
    X_unknown = (X_unknown - mean) / std_var

final_results = []
true_positives = 0
true_negatives = 0
false_positives = 0
false_negatives = 0
for author in np.unique(authors):
    result = results[authors == author][0]
    same_author = X_known[authors == author]
    unknown_text = X_unknown[authors == author][0]

    # Draw random opposition.
    random = traindata[np.random.choice(traindata.shape[0],
        same_author.shape[0], replace=False)]

    # Stack author specific and random.
    X_train = np.vstack([same_author, random])
    y_train = np.array([1] * same_author.shape[0] + [0] * same_author.shape[0])

    model = SVC(C=args.c, kernel='rbf', gamma=args.gamma)
    model.fit(X_train, y_train)

    prediction = model.predict(unknown_text.reshape(1, -1))[0]

    final_results.append(prediction == result)

    if prediction == result and result:
        true_positives += 1
    elif prediction == result and not result:
        true_negatives += 1
    elif prediction != result and result:
        false_negatives += 1
    else:
        false_positives += 1

print(np.sum(final_results) / float(len(final_results)), true_positives,
        true_negatives, false_positives, false_negatives)
