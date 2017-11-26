from collections import Counter

def find_ngrams(filepath, n):
    # Gets the character n-grams of provided texts, and
    # returns counts
    # Arg, filepath: Path to file, to be read. String
    # Arg, n: Which kind of gram to compute
    # Returns counting dictionary, of the n-grams contained in file

    fileStr = open(filepath, "r").read()
    a = [fileStr[i:i+n][:] for i in range(len(fileStr)-n)]
    return Counter(a)

