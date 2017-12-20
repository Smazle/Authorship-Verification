#!/usr/bin/python3

import argparse
from feature_extractor import analyse_input_folder, FeatureExtractor

# Parse command line arguments.
parser = argparse.ArgumentParser(description='Extract features.')

parser.add_argument(
    'data_folder', type=str, help='Location of data to run with.')

parser.add_argument(
    'outfile', type=str, help='File to write features to.')

parser.add_argument(
    '--master-file', type=str, default=None,
    help='File to write features of full text to.')

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

args = parser.parse_args()

# Convert arguments to description of features.
size = args.char_n_gram_size
character_grams = list(map(lambda x: (x, size), args.char_n_gram))

size = args.special_n_gram_size
special_grams = list(map(lambda x: (x, size), args.special_n_gram))

word_frequencies = args.word_frequencies

size = args.postag_n_gram_size
postag_grams = list(map(lambda x: (x, size), args.postag_n_gram))

size = args.word_n_gram_size
word_grams = list(map(lambda x: (x, size), args.word_n_gram))

# Get authors from input folder.
authors = analyse_input_folder(args.data_folder)

feature_extractor = FeatureExtractor(
    authors,
    character_grams=character_grams,
    special_character_grams=special_grams,
    word_frequencies=word_frequencies,
    postag_grams=postag_grams,
    word_grams=word_grams)

feature_extractor.extract(args.outfile, args.master_file)
