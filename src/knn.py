#!/usr/bin/python3

import sys
import numpy as np
from sklearn import neighbors

if len(sys.argv) != 2:
    print('Error')
    exit(1)

datafile = sys.argv[1]

X = np.loadtxt(datafile, dtype=np.float)
y = X[:,-1].astype(np.bool)

feature_n = X.shape[1] - 1
feature_n_half = int(feature_n / 2)

X_known = X[:,0:feature_n_half]
X_unknown = X[:,feature_n_half:-1]

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
