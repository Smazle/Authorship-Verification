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
