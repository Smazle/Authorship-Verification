#!/usr/bin/python3
from collections import Counter
import string


def words(filepath):
    fileStr = open(filepath, 'r').read()
    fileStr = ''.join([char if char in string.ascii_letters or
                       char == ' ' else ' ' for char in fileStr])

    ret = Counter(fileStr.split(' '))
    del ret['']
    return ret
