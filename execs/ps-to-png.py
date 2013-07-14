#!/usr/bin/env python
# encoding: utf-8
"""
File:        ps-to-png.py
Created:     Sunday, 14 July 2013
Author:      Sergey Vinokurov
Description:
"""

from __future__ import print_function, division

import argparse
import subprocess

parser = argparse.ArgumentParser(
    description = "convert ps file (presumably with single image/formula) to png",
    formatter_class = argparse.RawDescriptionHelpFormatter)

parser.add_argument("--transparent", dest = "transparent",
                    required = False, default = None,
                    help =
"""whether to make background transparent,
should be a color to make transparent in format rgb:rr/gg/bb""")
parser.add_argument("-d", "--dpi", dest = "dpi",
                    required = False, default = 150, type = int,
                    help = "resolution, dots per inch")

parser.add_argument("source", help = "source PS file")
parser.add_argument("dest", help = "destination png file")

args = parser.parse_args()

DEBUG = False

# scale artifically increases dpi so that image can be downlampled later
# to improve quality, becasue 1:1 dpi leads to ugly formulas
scale = 0.5

opt_transparency = \
    "-transparent " + args.transparent if args.transparent is not None else ""

cmd = ["echo quit", # use this echo because of gs quirk
       "gs -q -dNOPAUSE -r{dpi}x{dpi} -sOutputFile=- -sDEVICE=pnmraw {source} 2>/dev/null".format(
           dpi = int(args.dpi / scale),
           source = args.source),
       "pnmcrop {} -".format("-verbose" if DEBUG else ""),
       "pnmdepth 255",
       "pnmscale {}".format(scale),
       "pnmtopng -interlace {opt_transparent} >{dest}".format(
           dest = args.dest,
           opt_transparent = opt_transparency)]
subprocess.call(" | ".join(cmd), shell = True)


