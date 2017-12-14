#!/usr/bin/python3

from character import CharacterNGramFeatureExtractor,\
    SpecialCharacterNGramFeatureExtractor
from posTag import PosTagNGramsExtractor
from words import WordFrequencyExtractor, WordNGramsFeatureExtractor
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
prev = []
with open(FULL_TEXT_FILE, 'w') as outfile:
    for fname in files:
        with open(fname, 'r') as infile:
            text = infile.read()
            if args.type.lower() != "pancho" and text not in prev:
                outfile.write(text)
                prev.append(text)
            else:
                outfile.write(text)

# Parse command line arguments.
parser = argparse.ArgumentParser(description='Extract features.')

parser.add_argument(
    '--char-n-gram', type=int, nargs='+', default=[],
    help='n-gram sizes to extract.')

parser.add_argument(
    '--char-n-gram-size', type=int, nargs='?', default=0,
    help='Number of character n-grams to use. If n then the n most frequent ' +
    'n-grams are used. If multiple values are specified on --n-gram then n ' +
    'features are extracted for all of them.')

parser.add_argument(
    '--special-n-gram', type=int, nargs='+', default=[],
    help='special n-gram sizes to extract')

parser.add_argument(
    '--special-n-gram-size', type=int, nargs='?', default=0,
    help='Number of special n-grams to use. If n then the n most frequent ' +
    'special n-grams are used.')

parser.add_argument(
    '--word-frequencies', type=int, nargs='?', default=0,
    help='Number of most frequent words to count.')

parser.add_argument(
    '--postag-n-gram', type=int, nargs='+', default=[],
    help='What grams should be used when extracting pos tags')

parser.add_argument(
    '--postag-n-gram-size', type=int, nargs='?', default=0,
    help='The number of most common n-grams')

parser.add_argument(
    '--word-n-gram', type=int, nargs='+', default=[],
    help='Sizes of word n-grams to use.')

parser.add_argument(
    '--word-n-gram-size', type=int, nargs='?', default=0,
    help='Number of most frequent word n-grams to use.')

parser.add_argument(
    '--type', type=str, nargs='?', default='Normal',
    help="The type of output file to produce.")

args = parser.parse_args()

extractors = []
with open(FULL_TEXT_FILE, 'r') as f:
    content = f.read()

    # Handle character n-grams.
    for n in args.char_n_gram:
        extractor = CharacterNGramFeatureExtractor(n, args.char_n_gram_size)
        extractor.fit(content)

        extractors.append(extractor)

    # Handle special character n-grams.
    for n in args.special_n_gram:
        extractor = SpecialCharacterNGramFeatureExtractor(n,
                args.special_n_gram_size)
        extractor.fit(content)

        extractors.append(extractor)

    # Handle word frequencies.
    if args.word_frequencies != 0:
        extractor = WordFrequencyExtractor(args.word_frequencies)
        extractor.fit(content)

        extractors.append(extractor)

    # Handle POS tagging n-grams.
    for n in args.postag_n_gram:
        extractor = PosTagNGramsExtractor(n, args.postag_n_gram_size)
        extractor.fit(content)

        extractors.append(extractor)

    # Handle word n-grams.
    for n in args.word_n_gram:
        extractor = WordNGramsFeatureExtractor(n, args.word_n_gram_size)
        extractor.fit(content)

        extractors.append(extractor)


feature_extractor = FeatureExtractor(extractors)

# Generate features for each author.
authorFeatures = []
texts = []
with open(DATA_FOLDER + '/truth.txt') as truth_f:
    for a in range(1, 101):
        truth = truth_f.readline().endswith('Y\n')

        known_file = DATA_FOLDER + '/EN%03d/known01.txt' % a
        unknown_file = DATA_FOLDER + '/EN%03d/unknown.txt' % a

        known_features = feature_extractor.extract(known_file)
        unknown_features = feature_extractor.extract(unknown_file)

        features = known_features + unknown_features + [truth]

        if args.type.lower() != "pancho" and known_file not in texts:
            authorFeatures.append(features)
        else:
            authorFeatures.append(features)

if args.type.lower() == "pancho":
    master = feature_extractor.extract(FULL_TEXT_FILE)
    np.savetxt('outfile.txt', np.array(authors))
    np.savetxt('fullOut.txt', np.array([master]))
    print("Data saved to fullOut.txt")
else:
    np.savetxt('outfile.txt', np.array(authors))
    
print("Data saved to outfile.txt")

