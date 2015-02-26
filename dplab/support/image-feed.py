#!/usr/bin/env python

## parse a graphic file (of most known formats) and feed into Standard ML

import os, sys
import Image
from struct import *

fileName = sys.argv[1]
try:
    im = Image.open(fileName)
    w,h = im.size
    sys.stdout.write(pack("ii",w,h)) ## 4-byte ints for width and height
    rgbImg = im.convert("RGB")
    pixelStream = rgbImg.getdata()
    for (r,g,b) in pixelStream:
        sys.stdout.write(pack("BBB", r,g,b)) ## 1-byte each for r,g,b
except IOError, e:
    print >> sys.stderr, "%s: %s\n\nCannot open/understand %s" % (sys.argv[0], str(e), fileName)

