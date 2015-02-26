#!/usr/bin/env python

## unpack raw-file format from Standard ML into a JPEG output file

import os, sys
import Image
from struct import *

fileName = sys.argv[1]
try:
    ## Read/unpack 4-byte ints for width and height
    header = sys.stdin.read(2*4)
    w,h = unpack("ii", header)

    data = sys.stdin.read(3*w*h)
    im = Image.fromstring("RGB", (w, h), data, "raw", "RGB", 0, 1)
    im.save(fileName, "JPEG")
except IOError, e:
    print >> sys.stderr, "%s: %s\n\nCannot open/write to %s" % (sys.argv[0], str(e), fileName)

