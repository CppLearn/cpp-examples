#!/usr/bin/env python

import os, sys

fname = sys.argv[1]
pdf  = '%s.pdf' % fname

if os.path.exists(pdf):
    os.unlink(pdf)

cmd = 'pdflatex %s' % fname
os.system(cmd)
cmd = 'evince %s.pdf' % fname
os.system(cmd)


