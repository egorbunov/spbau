#! /usr/bin/env python3

import sys
import re
import pymorphy2
from collections import Counter
import csv


def solve(book_name):
    counts = {
        'ADJF': Counter(),
        'ADJS': Counter(),
        'ADVB': Counter(),
        'COMP': Counter(),
        'CONJ': Counter(),
        'GRND': Counter(),
        'INFN': Counter(),
        'INTJ': Counter(),
        'NOUN': Counter(),
        'NPRO': Counter(),
        'NUMR': Counter(),
        'PRCL': Counter(),
        'PRED': Counter(),
        'PREP': Counter(),
        'PRTF': Counter(),
        'PRTS': Counter(),
        'VERB': Counter()
    }

    morph_analyzer = pymorphy2.MorphAnalyzer()
    with open(book_name, 'r') as f:
        for w in re.split('\W+', ' '.join(f.readlines())):
            parse = morph_analyzer.parse(w)
            for x in parse:
                if x.score >= 0.1 and x.tag.POS:
                    counts[x.tag.POS][x.normal_form] += 1

    for tag in counts:
        most_common = counts[tag].most_common(10)
        with open(tag + '.csv', 'w') as f:
            csv_writer = csv.writer(f)
            csv_writer.writerows(most_common)


if __name__ == '__main__':
    solve(sys.argv[1])
