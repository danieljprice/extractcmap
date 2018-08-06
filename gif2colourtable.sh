#!/bin/bash
#
# extracts colour table from gif file
# wrapper for readppm.f90
#
if [ $# != 1 ]; then
   echo Usage: $0 file.gif;
else
   giffile=$1;
# convert gif to (temporary) pnm file
   giftopnm $giffile > crap.pnm;
# convert (temporary) pnm file to plain pnm format
   pnmfile=${giffile/.gif/.pnm};
   echo writing $pnmfile;
   pnmtoplainpnm crap.pnm > $pnmfile;
   ./getcmap $pnmfile;
fi
