#!/usr/bin/python3

import argparse
import numpy as np
from numpy.random import choice
from sklearn.svm import SVC
from sklearn.model_selection import LeaveOneOut
from sklearn.model_selection import GridSearchCV


class Hashable:

    def __init__(self, d):
        self.d = d

    def __hash__(self):
        return hash(str(self.d))


# Set up arguments
parser = argparse.ArgumentParser(
    description="Run author specific SVM classifier on input data given.")

parser.add_argument('file', type=str, help='Data File Location')

parser.add_argument(
    '--with-normalization',
    help='Whether or not to normalize data.',
    action='store_true',
    default=False)

parser.add_argument(
    '--c',
    help='Hyperparameter for SVM',
    type=float,
    default=None)

parser.add_argument(
    '--gamma',
    help='Hyperparamter for SVM',
    type=float,
    default=None)

parser.add_argument(
    '--with-PN',
    help='Whether or not to also print True Positive, False Positive, True ' +
    'Negative and False Negative.',
    action='store_true',
    default=False)

args = parser.parse_args()

# Remove author number
data = np.loadtxt(args.file, dtype=np.float)
authors = data[:, -1].astype(np.int)
results = data[:, -2].astype(np.bool)
X = data[:, :-2]

# Normalize the data.
if args.with_normalization:
    mean = np.mean(X, axis=0)
    std_var = np.std(X, axis=0)

    std_var[std_var == 0] = 1

    X = (X - mean) / std_var

if args.c is None or args.gamma is None:
    configurations = {}
    for author in np.unique(authors):
        result = results[authors == author][0]
        same_author = X[authors == author, 0:int(X.shape[1] / 2)]
        different_author = X[authors != author, 0:int(X.shape[1] / 2)]
        X_unknown = X[authors == author, int(X.shape[1] / 2):][0]

        # Draw random opposition.
        same_author_n = same_author.shape[0]
        random = different_author[choice(different_author.shape[0],
                                  same_author_n, replace=False), :]

        # Stack author specific and random.
        X_train = np.vstack([same_author, random])
        y_train = np.array([1] * same_author_n + [0] * same_author_n)

        # Cross validation over all C and gamma.
        C_range = np.logspace(-2, 10, 7)
        gamma_range = np.logspace(-9, 3, 7)
        param_grid = dict(gamma=gamma_range, C=C_range)
        cv = LeaveOneOut()
        grid = GridSearchCV(SVC(kernel='rbf'), param_grid=param_grid, cv=cv)
        grid.fit(X_train, y_train)

        try:
            configurations[Hashable(grid.best_params_)] += 1
        except KeyError:
            configurations[Hashable(grid.best_params_)] = 1

    # Extract best configuration from the above search.
    best_conf = max(configurations, key=configurations.get).d
else:
    best_conf = {'C': args.c, 'gamma': args.gamma}

final_results = []
true_positives = 0
true_negatives = 0
false_positives = 0
false_negatives = 0
for author in np.unique(authors):
    result = results[authors == author][0]
    same_author = X[authors == author, 0:int(X.shape[1] / 2)]
    different_author = X[authors != author, 0:int(X.shape[1] / 2)]
    X_unknown = X[authors == author, int(X.shape[1] / 2):][0]

    # Draw random opposition.
    same_author_n = same_author.shape[0]
    random = different_author[choice(different_author.shape[0], same_author_n,
                              replace=False), :]

    # Stack author specific and random.
    X_train = np.vstack([same_author, random])
    y_train = np.array([1] * same_author_n + [0] * same_author_n)

    model = SVC(C=best_conf['C'], kernel='rbf', gamma=best_conf['gamma'])
    model.fit(X_train, y_train)

    prediction = model.predict(X_unknown.reshape(1, -1))[0]

    final_results.append(prediction == result)

    if prediction == result and result == 1:
        true_positives += 1
    elif prediction == result and result == 0:
        true_negatives += 1
    elif prediction != result and result == 1:
        false_negatives += 1
    else:
        false_positives += 1


print(np.sum(final_results) / float(len(final_results)))
if args.with_PN:
    print('TP', true_positives, 'TN', true_negatives, 'FP', false_positives,
          'FN', false_negatives)
