#!/usr/bin/python3

import argparse
import numpy as np
from numpy.random import choice
from sklearn.ensemble import RandomForestClassifier

# Set up arguments
parser = argparse.ArgumentParser(
    description="Run Random Forest on extracted features")

parser.add_argument('file', type=str, help='Data File Location')

args = parser.parse_args()


def encode(allText, known, unknown):
    return (np.power(known - unknown, 2) + 1) /\
        (np.power(allText - unknown, 2) + 1)


# Remove author number
data = np.loadtxt(args.file, dtype=np.float)
authors = data[:, -1].astype(np.int)
results = data[:, -2].astype(np.bool)
X = data[:, :-2]

final_results = []
for author in np.unique(authors):
    result = results[authors == author][0]
    same_author = X[authors == author, 0:int(X.shape[1] / 2)]
    different_author = X[authors != author, 0:int(X.shape[1] / 2)]
    X_unknown = X[authors == author, int(X.shape[1] / 2):][0]

    # Draw random opposition.
    same_author_n = same_author.shape[0]
    random = different_author[choice(different_author.shape[0], same_author_n,
                              replace=False), :]

    print (author, result, X_unknown.shape, same_author.shape)

    # Stack author specific and random.
    X_train = np.vstack([same_author, random])
    y_train = np.array([result] * same_author_n + [not result] * same_author_n)

    model = RandomForestClassifier(n_estimators=2)
    model.fit(X_train, y_train)

    prediction = model.predict(X_unknown.reshape(1, -1))[0]

    final_results.append(prediction == result)

print(np.sum(final_results) / float(len(final_results)))