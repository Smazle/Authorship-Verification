#!/usr/bin/python3

from character import CharacterNGramFeatureExtractor,\
    SpecialCharacterNGramFeatureExtractor
from words import WordFrequencyExtractor
import argparse
import glob
import numpy as np


class FeatureExtractor:

    def __init__(self, extractors):
        self.extractors = extractors

    def extract(self, f):
        features = []
        with open(f) as f:
            content = f.read()

            for extractor in self.extractors:
                features = features + extractor.extract(content)

        return features


# Location of training data.
DATA_FOLDER = './data/'

# Path to a file that will be created containing a concatenation of all files
# in the training data set.
FULL_TEXT_FILE = './full_text.txt'

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

parser.add_argument('--n-gram-size', type=int, nargs='?',
                    help='Number of character n-grams to use. If n then the ' +
                    'n most frequent n-grams are used. If multiple values ' +
                    'are specified on --n-gram then n features are ' +
                    'extracted for all of them.')

parser.add_argument('--special-n-gram', type=int, nargs='+',
                    help='special n-gram sizes to extract')

parser.add_argument('--special-n-gram-size', type=int, nargs='?',
                    help='Number of special n-grams to use. If n then the ' +
                    'n most frequent special n-grams are used.')

parser.add_argument('--word-frequencies', type=int, nargs='?',
                    help='Number of most frequent words to count.')

# TODO: Support config of word n-grams.

# TODO: Support config of POS tagging n-grams.

args = parser.parse_args()

# Add default command line arguments.
if args.n_gram is None:
    args.n_gram = []
if args.special_n_gram is None:
    args.special_n_gram = []
if args.n_gram_size is None:
    args.n_gram_size = 500
if args.special_n_gram_size is None:
    args.special_n_gram_size = 5
if args.word_frequencies is None:
    args.word_frequencies = 0

extractors = []
with open(FULL_TEXT_FILE, 'r') as f:
    content = f.read()

    # Handle character n-grams.
    for n in args.n_gram:
        extractor = CharacterNGramFeatureExtractor(n, args.n_gram_size)
        extractor.fit(content)

        extractors.append(extractor)

    # Handle special character n-grams.
    for n in args.special_n_gram:
        extractor = SpecialCharacterNGramFeatureExtractor(n,
                args.special_n_gram_size)
        extractor.fit(content)

        extractors.append(extractor)

    if args.word_frequencies != 0:
        extractor = WordFrequencyExtractor(args.word_frequencies)
        extractor.fit(content)

        extractors.append(extractor)

    # TODO: Handle word n-grams.

    # TODO: Handle POS tagging n-grams.

feature_extractor = FeatureExtractor(extractors)

# Generate features for each author.
authors = []
with open(DATA_FOLDER + '/truth.txt') as truth_f:
    for a in range(1, 101):
        truth = truth_f.readline().endswith('Y\n')

        known_file = DATA_FOLDER + '/EN%03d/known01.txt' % a
        unknown_file = DATA_FOLDER + '/EN%03d/unknown.txt' % a

        known_features = feature_extractor.extract(known_file)
        unknown_features = feature_extractor.extract(unknown_file)

        features = known_features + unknown_features + [truth]

        # Add author class to feature vector.
        authors.append(features)

np.savetxt('outfile.txt', np.array(authors))
