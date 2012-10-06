#!/usr/bin/python3
import sys
from intelhex import IntelHex

outfile = open(sys.argv[1].rsplit(".", 1)[0] + ".rmh", 'w')
arr = IntelHex(sys.argv[1]).tobinarray()
if len(sys.argv) > 2:
    if sys.argv[2].endswith("k"):
        flen = int(sys.argv[2][:-1]) * 1024
    else:
        flen = int(sys.argv[2])
else:
    flen = len(arr)
    print("WARNING: Output size not specified.", file=sys.stderr)
for i in range(flen - len(arr)):
    arr.append(0)
for i, e in enumerate(arr):
    outfile.write("{0:02x}".format(e))
    outfile.write("\n")
