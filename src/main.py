#!/usr/bin/python3

from gram import find_ngrams
import glob

# Location of training data.
DATA_FOLDER = './data/'

# Path to a file that will be created containing a concatenation of all files
# in the training data set.
FULL_TEXT_FILE = './full_text.txt'

# Number of features to use for each n-gram.
GRAM_FEATURE_NUMBER = 500

# Construct text consisting of a concatenation of all texts in training data.
files = glob.glob(DATA_FOLDER + '/*/*.txt')
with open(FULL_TEXT_FILE, 'w') as outfile:
    for fname in files:
        with open(fname, 'r') as infile:
            outfile.write(infile.read())

# Find most frequent n-grams over all texts.
grams = find_ngrams(FULL_TEXT_FILE, 3)
print(grams.most_common(GRAM_FEATURE_NUMBER))
