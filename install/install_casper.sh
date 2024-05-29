#!/bin/sh
#  ------------------------------------------------------------------------
#  Install script for the NCAR DAV system 'casper'.
#
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
LIB=/glade/u/home/tcram/bufrlib/12.0.1/casper/gnu/lib64/libbufr_4.a  # path to BUFRLIB
EXE=../exe
INSTALL=.

cflag=""
fflag=""

if [ $CPLAT = linux ]
then
# Intel compiler
#   CC=icc
#   FC=ifort
#   fflag=""
#   cflag="-DUNDERSCORE"

# Gnu compiler
   FC=gfortran
   CC=gcc
   fflag=" -O3 -DUNDERSCORE -fno-second-underscore -w"
   cflag=" -O3 -DUNDERSCORE -w"
fi

#  Compile the source code
#  ---------------------------------------
 
echo "Compiling bufr_configdecode_ADPupa programs..."
$FC $fflag -c $SRC/dumpbufr.f
$FC $fflag -c $SRC/bufrupperair.f
 
#  link and load the executables
#  -----------------------------

echo "Linking..."
$FC $fflag -o $EXE/dumpbufr.x dumpbufr.o $LIB
$FC $fflag -o $EXE/bufrupperair.x bufrupperair.o $LIB

#  clean up
#  --------

rm -f *.o

echo "Finished."
