#!/usr/bin/python3

import sys
import numpy as np
import argparse
from sklearn.ensemble import RandomForestClassifier

parser = argparse.ArgumentParser(description=
        "Run Random Forest on extracted features")

parser.add_argument('file', type=str, help='Data File Location', nargs="+")
parser.add_argument('--split', type=float, nargs='?', help='The percentage of the data used for training')

args = parser.parse_args()
datafiles = args.file

if args.split is None:
    args.split = 100;

def encode(allText, known, unknown):
    return (np.power(known - unknown, 2) + 1) / (np.power(allText - unknown, 2) + 1)

X = np.loadtxt(datafiles[0], dtype=np.float)
y = X[:,-1]

feature_n = X.shape[1] - 1
feature_n_half = int(feature_n / 2)

X_known = X[:,0:feature_n_half]
X_unknown = X[:,feature_n_half:-1]
X = np.column_stack((X_known, X_unknown))
print(X.shape)

if len(datafiles) > 1:
    allText = np.loadtxt(datafiles[1], dtype=np.float)
    
    newX = []
    for k, uk in zip(X_known, X_unknown): 
        newX.append(encode(allText, k, uk))

    X = np.array(newX)    
    

np.random.shuffle(X)
boundary = int(np.floor(len(X) * args.split))

XInp = X[:boundary]
yInp = y[:boundary]

test = X[boundary:]
test_y = y[boundary:]

model = RandomForestClassifier()
model.fit(XInp, yInp)

predictions = model.predict(test) == test_y
print("Correct Random Forest", np.sum(predictions))

