#!/usr/bin/python3

from gram import CharacterNGramFeatureExtractor,\
    SpecialCharacterNGramFeatureExtractor
import argparse
import glob
import numpy as np


class FeatureExtractor:

    def __init__(self, extractors):
        self.extractors = extractors

    def extract(self, f):
        features = []
        with open(fname) as f:
            content = f.read()

            for extractor in self.extractors:
                features = features + extractor.extract(content)

        return features


# Location of training data.
DATA_FOLDER = './data/'

# Path to a file that will be created containing a concatenation of all files
# in the training data set.
FULL_TEXT_FILE = './full_text.txt'

# Number of features to use for each n-gram.
GRAM_FEATURE_NUMBER = 500
SPECIAL_GRAM_FEATURE_NUMBER = 5

# Construct text consisting of a concatenation of all texts in training data.
files = glob.glob(DATA_FOLDER + '/*/*.txt')
with open(FULL_TEXT_FILE, 'w') as outfile:
    for fname in files:
        with open(fname, 'r') as infile:
            outfile.write(infile.read())

# Parse command line arguments.
parser = argparse.ArgumentParser(description='Extract features.')
parser.add_argument('--n-gram', type=int, nargs='+',
                    help='n-gram sizes to extract.')
parser.add_argument('--special-n-gram', type=int, nargs='+',
                    help='special n-gram sizes to extract')
args = parser.parse_args()

# Add default command line arguments.
if args.n_gram is None:
    args.n_gram = []
if args.special_n_gram is None:
    args.special_n_gram = []

extractors = []
with open(FULL_TEXT_FILE, 'r') as f:
    content = f.read()

    for n in args.n_gram:
        extractor = CharacterNGramFeatureExtractor(n, GRAM_FEATURE_NUMBER)
        extractor.fit(content)

        extractors.append(extractor)

    for n in args.special_n_gram:
        extractor = SpecialCharacterNGramFeatureExtractor(n,
                SPECIAL_GRAM_FEATURE_NUMBER)
        extractor.fit(content)

        extractors.append(extractor)

feature_extractor = FeatureExtractor(extractors)

# Generate features for each author.
authors = []
for a in range(1, 101):
    fname = DATA_FOLDER + '/EN%03d/known01.txt' % a

    features = feature_extractor.extract(fname)

    # Add author class to feature vector.
    features.append(a)
    authors.append(features)

np.savetxt('outfile.txt', np.array(authors))
