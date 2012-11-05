#!/usr/bin/python3
import sys
from intelhex import IntelHex

outfile = open(sys.argv[1].rsplit(".", 1)[0] + ".rmh", 'w')
arr = IntelHex(sys.argv[1]).tobinarray()
outfile.write("@0000\n")
for i, e in enumerate(arr):
    outfile.write("{0:02x}\n".format(e))
