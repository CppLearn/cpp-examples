#!/usr/bin/env python

import os, sys

fname = sys.argv[1]
dvi = '%s.dvi' % fname
ps  = '%s.ps' % fname

if os.path.exists(dvi):
    os.unlink(dvi)
if os.path.exists(ps):
    os.unlink(ps)

cmd = 'latex %s' % fname
os.system(cmd)
cmd = 'dvips %s' % fname
os.system(cmd)
cmd = 'ps2pdf %s.ps' % fname
os.system(cmd)
cmd = 'evince %s.ps' % fname
os.system(cmd)

