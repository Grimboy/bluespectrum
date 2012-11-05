#!/usr/bin/python3
import sys, array, os

outfile = open(sys.argv[1].rsplit(".", 1)[0] + ".rmh", 'w')
infile = open(sys.argv[1], 'rb')
outfile.write("@0000\n")
for e in infile.read():
    outfile.write("{0:02x}\n".format(e))
