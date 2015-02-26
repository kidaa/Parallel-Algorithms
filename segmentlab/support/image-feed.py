#!/usr/bin/env python

## parse a graphic file (of most known formats) and feed into Standard ML

import os, sys
from PIL import Image
from struct import *

fileName = sys.argv[1]
try:
    im = Image.open(fileName)
    w,h = im.size
    print w
    print h
    rgbImg = im.convert("RGB")
    pixelStream = rgbImg.getdata()
    for (r,g,b) in pixelStream:
      print r, g, b
except IOError, e:
    print >> sys.stderr, "%s: %s\n\nCannot open/understand %s" % (sys.argv[0], str(e), fileName)

