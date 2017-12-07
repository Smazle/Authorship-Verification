from collections import Counter
from polyglot.text import Text


class PosTagNGramsExtractor:
    """
        Extracts the frequency of Part-Of-Speech(tag) n-grams
        from a text.

        The class is initialized with an interger denoting the number
         of most common postag n-grams to use from initialization and onwards.
         When fit against a text, the most common n-grams are extacted,
         which allowed for comparrison against a newly introduced text
         resulting in a list of pos-tag frequencies, matching the
         extracted most common n-grams.

         Attributes:
            size (int): Number of most frequent pos tag n-gram in text
            n (int): What number of gram should be extracted
            grams (list): A list of most frequent n-grams
    """
        

    def __init__(self, n, size):
        """
            Create a new instance the class, with the set size

            Args:
                size (int): The number of most common grams to extract
        """

        self.size = size
        self.n = n

    def fit(self, text):
        """
            Fit the extractor to a text, thus computing the set of 
            grams used for frequency computation on newly introduced texts

            Args:
                text (str): Text to generate out n-grams from
        """
        grams = nGramCount(text, self.n)
        most_common = grams.most_common(self.size)
        most_common = [key for (key, value) in most_common]

        self.grams = most_common

    def extract(self, text):
        """
            Gets the frequency of self.grams in the text provided

            Args:
                text (string): Text to get gram frequencies from

            Returns:
                list: Of type [int], containing the frequencies
        """

        return frequency(text, self.n, self.grams)


def nGramCount(text, n):
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

    posTagging = Text(text, hint_language_code="en")
    posTagging = [x[-1].encode("utf-8") for x in posTagging.pos_tags]
    posTagging = [tuple(posTagging[i:i + n][:])
                  for i in range(len(posTagging) - n)]
    return Counter(posTagging)


def frequency(text, n, ngrams):
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
            list: Of type [int], containing the frequencies
    """

    tags = nGramCount(text, n)
    total = float(len(tags))
    return [tags[key] / total if key in tags else 0
            for key in ngrams]
