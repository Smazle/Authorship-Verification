#!/usr/bin/env python3

import numpy as np
import matplotlib.pyplot as plt

data = np.loadtxt("../data/BrownPancoImportance2015.output", dtype=str, delimiter="\n")
data = [x.strip("'()").split(", ") for x in data]
data = [("".join(x[:-1]), x[-1]) for x in data]

idx = 0

if idx != 0:
    data = data[:idx]

    plt.bar(range(len(data)), [float(x[1]) for x in data])
    plt.xticks(range(len(data)), [x[0].replace("b\\","").replace("\\","") for x in data], rotation=30, horizontalalignment="right")
    
    plt.title(str(idx) + " most important features for UBM Random Forest")
    plt.xlabel("Text Feature")
    plt.ylabel("Importance of feature")
    plt.show()
else:
    plt.bar(range(len(data)), [float(x[1]) for x in data])
    plt.title("Feature importance for UBM Random Forest")
    plt.xlabel("Text Feature Index")
    plt.ylabel("Importance of feature")
    line = plt.plot(range(len(data)), [1/float(len(data)) for x in range(len(data))], color="red", label="Average Importance")
    plt.legend(handles=[line[0]])
    plt.show()
