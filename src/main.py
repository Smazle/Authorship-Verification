#!/usr/bin/python3

from gram import find_ngrams, find_frequencies
import argparse
import glob
import numpy as np

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

# Parse command line arguments.
parser = argparse.ArgumentParser(description='Extract features.')
parser.add_argument('--n-gram', type=int, nargs='+',
                    help='n-gram size to extract.')
args = parser.parse_args()

# Add default command line arguments.
if args.n_gram is None:
    args.n_gram = []

with open(FULL_TEXT_FILE, 'r') as f:
    content = f.read()

    # Extract a list of all the n-grams we are using for all n we are using.
    n_grams = []
    for n in args.n_gram:
        grams = find_ngrams(content, n)
        most_common = grams.most_common(GRAM_FEATURE_NUMBER)
        most_common = [key for (key, val) in most_common]

        n_grams.append((most_common, n))

# Generate features for each author.
authors = []
for i in range(1, 101):
    fname = DATA_FOLDER + '/EN%03d/known01.txt' % i
    features = []
    with open(fname) as f:
        content = f.read()

        for (grams, n) in n_grams:
            features = features + find_frequencies(content, grams, n)

    # Add author class to feature.
    features = features + [i]
    authors.append(features)

np.savetxt('outfile.txt', np.array(authors))
