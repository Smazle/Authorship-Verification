#!/usr/bin/python3
from collections import Counter
import string


def words(text):
    """
        Computes the number of words that occurs in the
        string supplied, by first removing reach charactor not in
        string.ascii_letters, and the splitting on a whitespace char

        Args:
            text (string): Text to be analysed for word count

        Returns:
            dictionary: A counting dicionary of type {word:count}
    """
    text = ''.join([char if char in string.ascii_letters or
                    char == ' ' else ' ' for char in text])

    ret = Counter(text.split(' '))
    del ret['']
    return ret


def wordFrequencies(text, wordsList):
    """
        Computes the frequencies of words present
        in a supplied text.

        Args:
            text (string): Text to be analysed for word frequencies
            wordsList (list): List containing the words one wants the
                frequencies of, in the form of strings

        Returns:
            dicionary: A frequencies dicionary of type {word:frequency}

    """
    wordCounts = words(text)
    return {key: value / float(len(wordCounts)) for
            key, value in wordCounts.iteritems() if key in wordsList}
