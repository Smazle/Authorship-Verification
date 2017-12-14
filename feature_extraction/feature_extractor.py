import os
from character import CharacterNGramFeatureExtractor,\
    SpecialCharacterNGramFeatureExtractor
from posTag import PosTagNGramsExtractor
from words import WordFrequencyExtractor, WordNGramsFeatureExtractor
import numpy as np


# TODO: description.
class FeatureExtractor:

    def __init__(
        self, authors, character_grams=[], special_character_grams=[],
        word_frequencies=0, postag_grams=[], word_grams=[], type='Normal'):

        self.authors = authors

        # If the type is Normal only unique files are used. If it is pancho
        # all files are concatenated.
        self.fulltext = gen_full_text(self.authors, type=='Normal')

        # Create feature extractors for the types of features requested.
        self.extractors = []
        # Handle character n-grams.
        for (n, size) in character_grams:
            extractor = CharacterNGramFeatureExtractor(n, size)
            extractor.fit(self.fulltext)

            self.extractors.append(extractor)

        # Handle special character n-grams.
        for (n, size) in special_character_grams:
            extractor = SpecialCharacterNGramFeatureExtractor(n, size)
            extractor.fit(self.fulltext)

            self.extractors.append(extractor)

        # Handle word frequencies.
        if word_frequencies != 0:
            extractor = WordFrequencyExtractor(word_frequencies)
            extractor.fit(self.fulltext)

            self.extractors.append(extractor)

        # Handle POS tagging n-grams.
        for (n, size) in postag_grams:
            extractor = PosTagNGramsExtractor(n, size)
            extractor.fit(self.fulltext)

            self.extractors.append(extractor)

        # Handle word n-grams.
        for (n, size) in word_grams:
            extractor = WordNGramsFeatureExtractor(n, size)
            extractor.fit(self.fulltext)

            self.extractors.append(extractor)

    def extract(self, outfile, master_file=None):
        # Generate features for each author.
        author_features = []
        for author in self.authors:
            unknown_features = self.extract_features(author.unknown)

            for known in author.known:
                known_features = self.extract_features(known)

                features = known_features + unknown_features +\
                    [author.truth] + [author.name]
                author_features.append(features)

        # Write features to file.
        np.savetxt(outfile, np.array(author_features))

        if master_file is not None:
            master_features = self.extract_features(self.fulltext)
            np.savetxt(master_file, np.array([master_features]))

    def extract_features(self, text):
        features = []

        for extractor in self.extractors:
            features = features + extractor.extract(text)

        return features


# TODO: description.
class Author:

    def __init__(self, name, known_files, unknown_file, truth):
        self.name = name
        self.truth = truth

        known = map(lambda x: open(x, 'r').read(), known_files)
        self.known = list(known)

        self.unknown = open(unknown_file, 'r').read()

    def __str__(self):
        return 'Author: ' + self.name


# TODO: description.
# Return [Author]
def analyse_input_folder(data_folder):
    all_fnames = os.listdir(data_folder)
    author_folders = filter(lambda x: x.startswith('EN'), all_fnames)
    truth_f = open(data_folder + '/truth.txt', 'r', encoding='utf-8-sig')\
        .read()

    authors = []
    for folder in author_folders:
        all_fnames = os.listdir(data_folder + '/' + folder)

        known = filter(lambda x: x.startswith('known'), all_fnames)
        known = map(lambda x: data_folder + '/' + folder + '/' + x, known)

        unknown = data_folder + '/' + folder + '/unknown.txt'

        truth = filter(lambda x: x.startswith(folder), truth_f.split('\n'))
        truth = list(truth)[0].endswith('Y')

        name = int(folder[2:])

        authors.append(Author(name, list(known), unknown, truth))

    return authors

def gen_full_text(authors, unique):
    all_texts = sum(map(lambda x: [x.unknown] + x.known, authors), [])

    if unique:
        return ''.join(set(all_texts))
    else:
        return ''.join(all_texts)
