#!/usr/bin/python3
from collections import Counter
import string


def Words(text):
    """
        Cleans up the text, removing characters not
        in string.ascii_letters, and get all the words
        after.

        Args:
            text (string): String of text to be cleaned up

        Returns:
            list[string]: A list of all
                the words contained in the supplied text
    """
    text = ''.join([char if char in string.ascii_letters or
                    char == ' ' else '' for char in text])

    return text.split(' ')


def Count(text):
    """
        Makes use of the Words function to get the list of
        words contained within the text, and then returns the
        total count.

        Args:
            text (string): Text to be analysed for word count

        Returns:
            dictionary: A counting dicionary of type {word:count}
    """
    return Counter(Words(text))


def Frequency(text, wordsList):
    """
        Computes the frequencies of words present
        in a supplied text.

        Args:
            text (string): Text to be analysed for word frequencies
            wordsList (list): List containing the words one wants the
                frequencies of, in the form of strings

        Returns:
            list (int): A list containing the frequencies of the words
                in wordsList, in the same order as wordsList

    """
    wordCounts = Count(text)
    total = float(len(wordCounts))
    return [wordCounts[key] / total if key in wordCounts else 0
            for key in wordsList]


def NGramCount(text, n, tolower):
    """
        Computes the count of the word n-grams
        that appear in the provided text.

        Args:
            text (string): Text to be analysed for word n-gram counts
            n (int): Which kind of gram to be used
            tolower (bool): Denotes of the text should be non-capitalized
                during the n-gram counting process
        Returns:
            dictionary: A counting dictionary of type {n-gram: count}

    """
    words = Words(text.lower()) if tolower else Words(text)
    print words
    grams = [tuple(words[i:i + n][:]) for i in range(len(words) - n + 1)]
    return Counter(grams)


def NGramFrequency(text, n, tolower, ngramList):
    """
        Computes the Frequency of a specified list
        of n-grams in the provided text string.

        Args:
            text (string): Text to be analysed for word n-gram counts
            n (int): Which kind of gram to be used
            tolower (bool): Denotes of the text should be non-capitalized
                during the n-gram counting process
            ngramList (list): A list of strings describing the word n-grams
                where a frequency is desired
        Returns:
            list[int]: A list containing the frequencies of supplied words
                in the order of ngramList
    """

    ngramCounts = NGramCount(text, n, tolower)
    total = float(len(ngramCounts))

    return [ngramCounts[key] / total if key in ngramCounts
            else 0 for key in ngramList]


print(Frequency("This is a test", ['a', 'test', "wow"]))
