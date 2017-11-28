#!/usr/bin/python3
from collections import Counter
def words(filepath):
    """
        Gets word count dictionary, of file. Removing everything
        but chars also in the string.ascii_letters list
        Arg, filepath: String describing filepath, of file to be counted
        Returns: Counting dictionary over all words in the text file provided
    """
    fileStr = open(filepath, "r").read(
    fileStr=''.join([char if char in string.ascii_letters
                       or char == " " else " " for char in fileStr])

    ret=Counter(fileStr.split(' '))
    del ret['']
    return ret
