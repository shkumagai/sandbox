#!/usr/bin/env python
# coding: utf-8
"""
sample implementation of Burrows Wheeler Transform (BWT).
"""

import sys


def bwtrans_encode(instr):
    str = instr + chr(0)
    array = [str]
    for n in range(len(str) - 1):
        array.append( (array[n])[1:] + (array[n])[0] )

    return ''.join([s[-1] for s in sorted(array)])


def bwtrans_decode(instr):
    length = len(instr)
    data = list(instr)
    pos = -1

    counts = []
    for i in xrange(0, sys.maxunicode):
        counts.append(0)

    lfmapping = []
    for j in xrange(0, length):
        lfmapping.append(0)
        if data[j] is chr(0):
            pos = j
        counts[ ord(data[j]) ] += 1

    for i in xrange(0, sys.maxunicode):
        counts[i] = counts[i] + counts[i-1]

    for p in range(length - 1, -1, -1):
        idx = counts[ ord(data[p]) ] - 1
        lfmapping[ idx ] = p
        counts[ ord(data[p]) ] = idx

    buf = []
    for p in range(0, length - 1):
        pos = lfmapping[pos]
        buf.append(data[pos])

    return ''.join(buf)


def main():
    original = sys.argv[1:]
    print "original : %s" % ' '.join(original)

    bwtstring = bwtrans_encode(' '.join(original))
    print "encoded  : %s" % bwtstring
    print "decoded  : %s" % bwtrans_decode(bwtstring)


if __name__ == '__main__':
    main()
