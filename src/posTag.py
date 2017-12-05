#!/usr/bin/python3
from collections import Counter
from polyglot.text import Text


def GetPosNGrams(text, n, language):
    """
        Computes the count of the pos tag n-grams of the
        text supplied.

        Args:
            text (string): The text to be analysed for
                its' pos tag n-gram count
            n (int): Denotes the grams to extract
            language (string): Describes which language polyglot should expect
                to receive when analysing the supplied string

        Returns:
            dictionary: A dictionary with the n-gram as the key in the form
                of a tuple, and the count as the value
    """

    posTagging = Text(text, hint_language_code=language)
    posTagging = [x[-1].encode("utf-8") for x in posTagging.pos_tags]
    posTagging = [tuple(posTagging[i:i + n][:])
                  for i in range(len(posTagging) - n)]
    return Counter(posTagging)


def GetFrequencies(text, n, language, ngrams):
    """
        Computes the frequencies of a list if supplied n-grams in
        a text provided, by dividing the number of n-gram occourence
        with the total number of n-grams.

        Args:
            text (string): The text to be analysed for its'
                pos tag n-gram count
            n (int): Denotes the grams to extract
            language (string): Describes which language polyglot should expect
                to receive when analysing the supplied string
            ngrams (list): A list containing the n-grams that one wants to
                know the frequency of. The ngrams are
                of type tuple(string, ..)

        Returns:
            dictionary: Of type {n-gram: frequency}
    """

    total = float(len(text) - n)
    tags = GetPosNGrams(text, n, language)
    return {key: (value / total) for key, value
            in tags.iteritems() if key in ngrams}
