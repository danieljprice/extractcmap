#!/bin/bash
#
# extracts colour table from png file
# wrapper for readppm.f90
#
if [ $# != 1 ]; then
   echo Usage: $0 file.png;
else
   giffile=$1;
# convert gif to (temporary) pnm file
   pngtopnm $giffile > crap.pnm;
# convert (temporary) pnm file to plain pnm format
   pnmfile=${giffile/.png/.pnm};
   echo writing $pnmfile;
   pnmtoplainpnm crap.pnm > $pnmfile;
   ./getcmap $pnmfile;
fi
