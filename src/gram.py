#!/usr/bin/python3

from collections import Counter
import sys
import string


class NGramFeatureExtractor:

    def __init__(self, n, size):
        self.n = n
        self.size = size

    def fit(self, full_text):
        grams = find_ngrams(full_text, self.n)
        most_common = grams.most_common(self.size)
        most_common = [key for (key, val) in most_common]

        self.grams = most_common

    def extract(self, text):
        return find_frequencies(text, self.grams, self.n)


class SpecialCharacterNGramFeatureExtractor:

    def __init__(self, n, size):
        self.n = n
        self.size = size

    def fit(self, full_text):
        grams = special_character_n_grams(full_text, self.n)
        most_common = grams.most_common(self.size)
        most_common = [key for (key, val) in most_common]

        self.grams = most_common

    def extract(self, text):
        return special_character_n_gram_frequencies(text, self.grams, self.n)


def n_gram_number(string_length, n):
    """
        Compute number of n-grams in a string of length string_length.
        Arg, string_length: Length of string.
        Arg, n: N-gram length.
        Returns number of n-grams.
    """
    return string_length - n


def find_ngrams(f, n):
    """
        Gets the character n-grams of provided texts, and
        returns counts
        Arg, f: File object or string object.
        Arg, n: Which kind of gram to compute.
        Returns counting dictionary, of the n-grams contained in file.
    """

    content = None
    try:
        content = f.read()
    except AttributeError:
        content = f

    n_grams = [content[i:i + n][:] for i in range(len(content) - n)]

    return Counter(n_grams)


def find_frequencies(f, n_grams, n):
    """
        Get frequencies of specific n-grams in the order they appear in the
        list.
        Arg, f: File object or string object.
        Arg, n_grams: List of n-grams to extract frequencies for.
        Arg, n: Lenght of n-grams.
        Returns The frequencies of each of the specific n-grams given.
    """

    content = None
    try:
        content = f.read()
    except AttributeError:
        content = f

    gram_n = n_gram_number(len(content), n)

    frequencies = [c / float(gram_n) for c in
                   (content.count(n_gram) for n_gram in n_grams)]

    return frequencies


def special_character_n_grams(f, n):
    content = None
    try:
        content = f.read()
    except AttributeError:
        content = f

    # Remove all non special characters.
    content = ''.join([c if c not in string.ascii_letters
                      and not c.isspace() else '' for c in content])

    return find_ngrams(content, n)


def special_character_n_gram_frequencies(f, n_grams, n):
    content = None
    try:
        content = f.read()
    except AttributeError:
        content = f

    content = ''.join([c if c not in string.ascii_letters
                      and not c.isspace() else '' for c in content])

    return find_frequencies(content, n_grams, n)


if __name__ == '__main__':
    print(find_ngrams(sys.stdin, 3))
