#!/usr/bin/python
import sys
from intelhex import IntelHex

basename = sys.argv[1].rsplit(".", 1)[0]
outfile = open(basename + ".rmh", 'w')
outfile_bin = open(basename + ".bin", 'wb')
ih = IntelHex(sys.argv[1])
arr = ih.tobinarray()
outfile.write("@0000\n")
for i, e in enumerate(arr):
    outfile.write("{0:02x}\n".format(e))
ih.tobinfile(outfile_bin)
