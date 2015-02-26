#!/usr/bin/env python

## unpack raw-file format from Standard ML into a JPEG output file

import os, sys
import re
from PIL import Image
from struct import *

fileName = sys.argv[1]
try:
    ## Read/unpack 4-byte ints for width and height
    w = int(sys.stdin.readline())
    h = int(sys.stdin.readline())

    def toTrip(s):
      non_decimal = re.compile(r'[^\d]+')
      r, g, b = map(lambda x: int(non_decimal.sub('', x)), s.split())
      return (r, g, b)
    data = map(toTrip, sys.stdin.readlines())

    im = Image.new("RGB", (w, h))
    im.putdata(data)
    im.save(fileName, "PNG")
except IOError, e:
    print >> sys.stderr, "%s: %s\n\nCannot open/write to %s" % (sys.argv[0], str(e), fileName)

