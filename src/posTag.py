from collections import Counter
from polyglot.text import Text


class PosTagNGramsExtractor:
    """
        Extracts the frequency of Part-Of-Speech n-grams
        from a text.
    """
        

    def __init__(self, n, size):
        self.size = size
        self.n = n

    def fit(self, text):
        grams = nGramCount(text, self.n)
        most_common = grams.most_common(self.size)
        most_common = [key for (key, value) in most_common]

        self.grams = most_common

    def extract(self, text):
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
