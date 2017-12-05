#!/usr/bin/python3

from collections import Counter
import sys
import string


class CharacterNGramFeatureExtractor:
    """
    Extract character n-grams from a text.

    The class is initialized with the size of the n-grams to extract and the
    number of n-grams to use. If only 500 n-grams are used only the most
    frequent ones are extracted.

    N-grams are sequences of characters in a text string. Consider the sentence
    `Hello John.`. All 3-grams in that sentence is,

    ["Hel", "ell", "llo", "lo ", "o J", " Jo", "Joh", "ohn", "hn."].

    After the feature extractor has been created it has to be fitted. The text
    it is fitted to is used to find the most common n-grams. Those `size` most
    common n-grams are then extracted from a specific text using the extract
    function.

    Attributes:
        n (int): Size of n-grams to extract.
        size (int): Number of different n-grams to extract.
    """

    def __init__(self, n, size):
        """
        Create new feature extractor.

        Args:
            n (int): Lenght of n-grams to use.
            size (int): Number of most frequent n-grams to use.
        """

        self.n = n
        self.size = size

    def fit(self, corpus):
        """
        Fit a feature extractor to a text corpus. The `size` most common
        n-grams are identified in the `corpus` text. These most common n-grams
        can then later be extracted by using the extract method.

        Args:
            corpus (file or str): Source of text corpus to find most common
                n-grams in.
        """

        grams = find_ngrams(corpus, self.n)
        most_common = grams.most_common(self.size)
        most_common = [key for (key, val) in most_common]

        self.grams = most_common

    def extract(self, text):
        """
        Extract `size` most common features as identified in `fit` from the
        `text`. Frequencies are computed by computing the total number of
        n-grams and the count of the most common and dividing the count with
        the total number of n-grams.

        Args:
            text (file or str): Source of text to extract frequencies for.

        Returns:
            list: List of frequencies in the order they appear in `self.grams`.
        """

        return find_frequencies(text, self.grams, self.n)


class SpecialCharacterNGramFeatureExtractor:
    """
    Extract special character n-grams from a text.

    The class is initialized with the size of the n-grams to extract and the
    number of n-grams to use. If only 500 n-grams are used only the most
    frequent ones are extracted.

    Special character N-grams are sequences of non alphanumeric and spacing
    characters in a text string. Consider the sentence `Hello John. How are
    you? I hope you are fine!`. All special character 3-grams in that sentence
    is,

    [".?!"].

    After the feature extractor has been created it has to be fitted. The text
    it is fitted to is used to find the most common n-grams. Those `size` most
    common n-grams are then extracted from a specific text using the extract
    function.

    Attributes:
        n (int): Size of n-grams to extract.
        size (int): Number of different special n-grams to extract.
    """

    def __init__(self, n, size):
        """
        Create new special character feature extractor.

        Args:
            n (int): Lenght of special n-grams to use.
            size (int): Number of most frequent n-grams to use.
        """

        self.n = n
        self.size = size

    def fit(self, full_text):
        """
        Fit a feature extractor to a text corpus. The `size` most common
        n-grams are identified in the `corpus` text. These most common n-grams
        can then later be extracted by using the extract method.

        Args:
            corpus (file or str): Source of text corpus to find most common
                n-grams in.
        """

        grams = special_character_n_grams(full_text, self.n)
        most_common = grams.most_common(self.size)
        most_common = [key for (key, val) in most_common]

        self.grams = most_common

    def extract(self, text):
        """
        Extract `size` most common features as identified in `fit` from the
        `text`. Frequencies are computed by computing the total number of
        n-grams and the count of the most common and dividing the count with
        the total number of n-grams.

        Args:
            text (file or str): Source of text to extract frequencies for.

        Returns:
            list: List of frequencies in the order they appear in `self.grams`.
        """

        return special_character_n_gram_frequencies(text, self.grams, self.n)


def n_gram_number(string_length, n):
    """
    Compute number of n-grams in a string of length string_length.

    Args:
        string_length (int): Length of string.
        n (int): N-gram length.

    Returns:
        int: Number of n-grams.
    """
    return string_length - n


def find_ngrams(f, n):
    """
    Gets the character n-grams of provided texts, and returns counts.

    Args:
        f (file or str): Source to find n-grams in.
        n (int): Which kind of gram to compute.

    Returns:
        dict: Counting dictionary, of the n-grams contained in file or string.
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
    Get frequencies of specific n-grams in the order they appear in the list
    given.

    Args:
        f (file or str): Source to find n-gram frequencies in.
        n_grams (list of str): List of n-grams to get frequencies for.
        n (int): Lenght of n-grams.

    Returns:
        list: The frequencies of each of the specific n-grams given.
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
    """
    Get n-gram counts of only special characters. Special characters are non
    alphanumeric non whitespace characters.

    Args:
        f (file or str): Source to find n-grams in.
        n (int): Lenght of n-grams.

    Returns:
        dict: The count of each of the specific n-grams given.
    """

    content = None
    try:
        content = f.read()
    except AttributeError:
        content = f

    # Remove all non special characters.
    content = ''.join(
        [c if c not in string.ascii_letters
            and not c.isspace()
            and c not in string.digits else '' for c in content])

    return find_ngrams(content, n)


def special_character_n_gram_frequencies(f, n_grams, n):
    """
    Get n-gram frequencies of only special characters. Special characters are
    non alphanumeric non whitespace characters.

    Args:
        f (file or str): Source to find n-gram frequencies in.
        n_grams (list): List of n-grams to find frequencies for.
        n (int): Lenght of n-grams.

    Returns:
        list: The frequencies of all input n-grams.
    """

    content = None
    try:
        content = f.read()
    except AttributeError:
        content = f

    # Remove all non special characters.
    content = ''.join(
        [c if c not in string.ascii_letters
            and not c.isspace()
            and c not in string.digits else '' for c in content])

    return find_frequencies(content, n_grams, n)


if __name__ == '__main__':
    print(find_ngrams(sys.stdin, 3))
