#!/usr/bin/python3

import sys
import numpy as np
import argparse
from sklearn.ensemble import RandomForestClassifier


# Set up arguments
parser = argparse.ArgumentParser(description=
        "Run Random Forest on extracted features")

parser.add_argument('file', type=str, help='Data File Location', nargs="+")
parser.add_argument('--split', type=float, nargs='?', help='The percentage of the data used for training')

args = parser.parse_args()
datafiles = args.file

if args.split is None:
    args.split = .5;

def encode(allText, known, unknown):
    return (np.power(known - unknown, 2) + 1) / (np.power(allText - unknown, 2) + 1)



# Remove author number
X = np.loadtxt(datafiles[0], dtype=np.float)[:, :-1]
y = X[:,-1]

feature_n = X.shape[1] - 1
feature_n_half = int(feature_n / 2)

X_known = X[:,0:feature_n_half]
X_unknown = X[:,feature_n_half:-1]

X = np.column_stack((X_known, X_unknown))


# If two files are supplied (One containing the total features against itself)
# then encode it, as per pancho.
if len(datafiles) > 1:
    allText = np.loadtxt(datafiles[1], dtype=np.float)
    
    newX = []
    for known, unknown in zip(X_known, X_unknown): 
        newX.append(encode(allText, known, unknown))

    X = np.array(newX)    
    

# Get training and test set split - Randomly
np.random.shuffle(X)
boundary = int(np.floor(len(X) * args.split))

XTrain = X[:boundary]
yTrain = y[:boundary]

XTest = X[boundary:]
yTest = y[boundary:]

# Create model
model = RandomForestClassifier()
model.fit(XTrain, yTrain)

predictions = model.predict(XTest) == yTest
print("Correct Random Forest", np.sum(predictions))

