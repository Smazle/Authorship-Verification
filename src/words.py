from collections import Counter
import string
import nltk


class WordFrequencyExtractor:
    """
    Extract word frequencies from a text.

    The class is initialized with number of most frequent words to use. After
    the feature extractor has been created it has to be fitted. To fit it give
    the text to fit to. The fitting text is used to identify the most common
    words. When features are then extracted from another text the most common
    words from the fitting process is the ones frequencies are computed for.

    Both the fitting text and the extraction text is preprocessed by removing
    all special characters from them before the words are found.

    Attributes:
        size (int): Number of most frequent words to compute frequencies for.
        words (list): List of most common words in the fitting text.
    """

    def __init__(self, size):
        """
        Create new feature extractor.

        Args:
            size (int): Number of most frequent words to compute frequencies
                for.
        """

        self.size = size

    def fit(self, text):
        """
        Fit a feature extractor to a text corpus. The `size` most common words
        are identified in the `corpus` text. These most common words can then
        later be extracted by using the extract method.

        Special character are removed from the text before the words are
        extracted.

        Args:
            corpus (str): Text corpus to find most common words in.
        """

        text = ''.join(
            [c if c in string.ascii_letters
                or c.isspace()
                or c in string.digits else '' for c in text])

        words = find_word_count(text)
        most_common = words.most_common(self.size)
        most_common = [key for (key, value) in most_common]

        self.words = most_common

    def extract(self, text):
        """
        Extract `size` most common features as identified in `fit` from the
        `text`. Frequencies are computed by computing the total number of words
        and the count of the most common and dividing the count with the total.

        Special character are removed before the frequencies are computed.

        Args:
            text (str): Text to extract features for.

        Returns:
            list: List of frequencies in the order they appear in `self.words`.
        """

        text = ''.join(
            [c if c in string.ascii_letters
                or c.isspace()
                or c in string.digits else '' for c in text])

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


def find_word_frequencies(text, words):
    """
        Computes the frequencies of words present
        in a supplied text.

        Args:
            text (string): Text to be analysed for word frequencies
            words (list): List containing the words one wants the
                frequencies of, in the form of strings

        Returns:
            list (int): A list containing the frequencies of the words
                in words, in the same order as words

    """

    total = float(len(find_words(text)))
    word_counts = find_word_count(text)

    return [word_counts[key] / total if key in word_counts else 0.0
            for key in words]


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
