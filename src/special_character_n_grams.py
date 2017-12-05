#!/usr/bin/python3

import sys
import string
from collections import Counter


def extract_n_grams(f, n):
    content = f.read()

    # Remove all non special characters.
    content = ''.join([c if c not in string.ascii_letters
        and not c.isspace() else '' for c in content])

    # Get n grams.
    n_grams = [content[i:i + n][:] for i in range(len(content) - n)]

    return Counter(n_grams)


if __name__ == '__main__':
    print(extract_n_grams(sys.stdin, 3))
