#!/bin/sh
#  ------------------------------------------------------------------------
#  This script will make bufrupperair.x which to extract data from ADP BUFR
#  input files, and place the data into a basic text file.  It is used to
#  extract data from ADPUPA, AIRCFT, SATWND, and AIRCAR BUFR files:
#      gdas.adpupa.tHHz.YYYYMMDD.bufr 
#      gdas.aircft.tHHz.YYYYMMDD.bufr
#      gdas.satwnd.tHHz.YYYYMMDD.bufr 
#      gdas.aircar.tHHz.YYYYMMDD.bufr
#
#  dumpbufr.x:        used to dump all contents of a BUFR file.
#
#  Note: NCEP BUFRLIB library is required to compile this source code,
#        and the location of your local installation of BUFRLIB should
#        be defined in the 'LIB' variable below.  The NCEP BUFRLIB 
#        software is available at
#        https://emc.ncep.noaa.gov/emc/pages/infrastructure/bufrlib.php
#  ------------------------------------------------------------------------
 
set -eua
 
#  ------------------------------------------------------------------------
#  CPLAT - platform type (linux,sgi,aix,sun)
#  ------------------------------------------------------------------------
 
CPLAT=linux
SRC=../src
EXE=../exe
LIB=/path/to/bufrlib.a
INSTALL=.

cflag=""
fflag=""

if [ $CPLAT = linux ]
then
   export FC=gfortran
   export CC=gcc
   fflag=" -O3 -DUNDERSCORE -fno-second-underscore -w"
   cflag=" -O3 -DUNDERSCORE -w"
fi

#  Compile the decode programs
#  ---------------------------------------
 
echo "Compiling bufr_configdecode_ADPupa programs..."
$FC $fflag -c $SRC/dumpbufr.f
$FC $fflag -c $SRC/bufrupperair.f
 
#  link and load the executables
#  -----------------------------

echo "Linking..."
$FC $fflag -o $EXE/dumpbufr.x dumpbufr.o $LIB/bufrlib.a
$FC $fflag -o $EXE/bufrupperair.x bufrupperair.o $LIB/bufrlib.a

#  clean up
#  --------

rm -f *.o

echo "Finished."
