import numpy as np
import itertools
import sys
import os

args = np.loadtxt('./args', dtype=str)


argsSize = [x for x in args if 'size' in x]
args = [x for x in args if x not in argsSize]


def GenArgs(args, argsSize):
    callStrings = []
    callString = ''

    for i in range(2, len(args)):
        arg_selected = list(itertools.combinations(args, i))

        for combo in arg_selected:

            params = []
            for i in range(1, 6):
                params.append(i)
                for size in range(20, 120, 20):

                    for arg in combo:

                        callString += ' ' + arg
                        callString += ' ' + ' '.join(str(x) for x in params)

                        sizeParam = next(x for x in argsSize if arg in x)

                        callString += ' ' + sizeParam
                        callString += ' ' + str(size)

                    callStrings.append(callString)
                    callString = ''

    return callStrings


strings = GenArgs(args, argsSize)
print len(strings)

for arg in strings:
    cmd = '../../feature_extraction/main.py ../../data/pan_2015 out'
    cmd += ' --master-file all' + arg
    os.system(cmd)

    cmd = '../../machine_learning/forest.py out all --split 0.8 --method ' \
        + sys.argv[1]

    os.system(cmd)
