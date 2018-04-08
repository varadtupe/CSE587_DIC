#!/usr/bin/env python
"""A more advanced Reducer, using Python iterators and generators."""

from itertools import groupby
from operator import itemgetter
import sys

def read_mapper_output(file, separator=','):
    for line in file:
        yield line.rstrip().split(separator, 1)


def main(separator=','):
    word_dict = {}	
    
    data = read_mapper_output(sys.stdin, separator=separator)

    for current_word, group in groupby(data, itemgetter(0)):
        try:
            value = sum(int(count) for current_word, count in group)
	    if current_word not in word_dict:
                word_dict[current_word] = value
            else:
                word_dict[current_word] += value

            # print "%s%s%d" % (current_word, separator, total_count)
        except ValueError:
            # count was not a number, so silently discard this item
            pass
    
    #Get sorted list of keys
    keys = [(k, word_dict[k]) for k in sorted(word_dict, key=word_dict.get, reverse=True)]
    print("text,size")
#Print each word with its count
    for k, v in keys[0:100]:
        print("%s%s%d" % (k,separator, word_dict[k]))    
if __name__ == "__main__":
    main()