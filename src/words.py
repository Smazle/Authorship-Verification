from collections import Counter
import string
import nltk


class CountFeatureExtractor:

    def __init__(self, size):
        self.size = size

    def fit(self, text):
        words = find_word_count(text)
        most_common = words.most_common(self.size)
        most_common = [key for (key, value) in most_common]

        self.words = most_common

    def extract(self, text):
        return find_word_frequencies(text, self.words)


class NGramsFeatureExtractor:

    def __init__(self, n, size):
        self.n = n
        self.size = size

    def fit(self, text):
        grams = find_word_n_grams(text, self.n)
        most_common = grams.most_common(self.size)
        most_common = [key for (key, value) in most_common]

        self.grams = most_common

    def extract(self, text):
        return find_word_n_gram_frequencies(text, self.n, self.grams)


def find_words(text):
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
    return nltk.word_tokenize(text)


def find_word_count(text):
    """
        Makes use of the find_words function to get the list of
        words contained within the text, and then returns the
        total count.

        Args:
            text (string): Text to be analysed for word count

        Returns:
            dictionary: A counting dicionary of type {word:count}
    """
    return Counter(find_words(text))


def find_word_frequencies(text, wordsList):
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
    wordCounts = find_word_count(text)
    total = float(len(wordCounts))
    return [wordCounts[key] / total if key in wordCounts else 0
            for key in wordsList]


def find_word_n_grams(text, n):
    """
        Computes the count of the word n-grams
        that appear in the provided text.

        Args:
            text (string): Text to be analysed for word n-gram counts
            n (int): Which kind of gram to be used
        Returns:
            dictionary: A counting dictionary of type {n-gram: count}

    """
    words = find_words(text.lower())
    grams = [tuple(words[i:i + n][:]) for i in range(len(words) - n + 1)]
    return Counter(grams)


def find_word_n_gram_frequencies(text, n, n_grams):
    """
        Computes the find_word_frequencies of a specified list
        of n-grams in the provided text string.

        Args:
            text (string): Text to be analysed for word n-gram counts
            n (int): Which kind of gram to be used
            n_grams (list): A list of strings describing the word n-grams
                where a frequency is desired
        Returns:
            list[int]: A list containing the frequencies of supplied words
                in the order of n_grams
    """

    n_gram_counts = find_word_n_grams(text, n)
    total = float(len(n_gram_counts))

    return [n_gram_counts[key] / total if key in n_gram_counts
            else 0 for key in n_grams]
