import os
from character import CharacterNGramFeatureExtractor,\
    SpecialCharacterNGramFeatureExtractor
from posTag import PosTagNGramsExtractor
from words import WordFrequencyExtractor, WordNGramsFeatureExtractor
import numpy as np
from sklearn.preprocessing import scale
from nltk.corpus import brown


# TODO: description.
class FeatureExtractor:

    def __init__(
        self, authors, character_grams=[], special_character_grams=[],
            word_frequencies=0, postag_grams=[], word_grams=[], corpus='all',
            normalize=True, feature_header=None):

        self.authors = authors
        self.normalize = normalize
        self.feature_header = feature_header

        # If the type is Normal only unique files are used. If it is pancho
        # all files are concatenated.
        self.fulltext = gen_full_text(self.authors, corpus)

        # Create feature extractors for the types of features requested.
        self.extractors = []
        self.featureNames = []

        def feat_name(x, y, n): return [x.format(
            n, size + 1) for size in range(y)]

        # Handle character n-grams.
        for (n, size) in character_grams:
            extractor = CharacterNGramFeatureExtractor(n, size)
            extractor.fit(self.fulltext)

            self.extractors.append(extractor)
            self.featureNames += feat_name('character_grams-{}-{}', size, n)

        # Handle special character n-grams.
        for (n, size) in special_character_grams:
            extractor = SpecialCharacterNGramFeatureExtractor(n, size)
            extractor.fit(self.fulltext)

            self.extractors.append(extractor)
            self.featureNames += feat_name(
                'special_character_grams-{}-{}', size, n)

        # Handle word frequencies.
        if word_frequencies != 0:
            extractor = WordFrequencyExtractor(word_frequencies)
            extractor.fit(self.fulltext)

            self.extractors.append(extractor)
            self.featureNames += feat_name('word_frequencies-{}{}',
                                           word_frequencies, '')

        # Handle POS tagging n-grams.
        for (n, size) in postag_grams:
            extractor = PosTagNGramsExtractor(n, size)
            extractor.fit(self.fulltext)

            self.extractors.append(extractor)
            self.featureNames += feat_name('postag_grams-{}-{}', size, n)

        # Handle word n-grams.
        for (n, size) in word_grams:
            extractor = WordNGramsFeatureExtractor(n, size)
            extractor.fit(self.fulltext)

            self.extractors.append(extractor)
            self.featureNames += feat_name('word_grams-{}-{}', size, n)

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
        author_features = np.array(author_features)
        if self.normalize:
            author_features[:, :-2] = scale(author_features[:, :-2], axis=0)

        np.savetxt(outfile, author_features)

        if master_file is not None:
            master_features = self.extract_features(self.fulltext)
            np.savetxt(master_file, np.array([master_features]))

        if self.feature_header is not None:
            open(self.feature_header, 'w').write(' '.join(self.featureNames))

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


def gen_full_text(authors, corpus):
    if corpus == 'all':
        all_texts = sum([x.known for x in authors], [])

        return ''.join(set(all_texts))
    elif corpus == 'brown':
        paragraphs = brown.paras()

        paragraph_txt = ''
        for paragraph in paragraphs:

            sentence_txt = ''
            for sentence in paragraph:

                word_txt = ''
                for word in sentence:
                    if word == '.' or word == ',' or word == '!'\
                            or word == '?':
                        word_txt = word_txt[:-1] + word + ' '
                    else:
                        word_txt += word + ' '

                sentence_txt += word_txt

            paragraph_txt += sentence_txt + '\n\n'

        return paragraph_txt
    else:
        raise Exception('UNKNOWN CORPUS')
