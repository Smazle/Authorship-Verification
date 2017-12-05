#!/usr/bin/python3

from collections import Counter
import sys


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
        Arg, f: File object.
        Arg, n: Which kind of gram to compute.
        Returns counting dictionary, of the n-grams contained in file.
    """

    content = f.read() if type(f) is file else f
    n_grams = [content[i:i + n][:] for i in range(len(content) - n)]

    return Counter(n_grams)


def find_frequencies(f, n_grams, n):
    """
        Get frequencies of specific n-grams in the order they appear in the
        list.
        Arg, f: File object.
        Arg, n_grams: List of n-grams to extract frequencies for.
        Arg, n: Lenght of n-grams.
        Returns The frequencies of each of the specific n-grams given.
    """

    content = f.read() if type(f) is file else f
    gram_n = n_gram_number(len(content), n)

    frequencies = [c / float(gram_n) for c in
                   (content.count(n_gram) for n_gram in n_grams)]

    return frequencies


if __name__ == '__main__':
    print(find_ngrams(sys.stdin, 3))
