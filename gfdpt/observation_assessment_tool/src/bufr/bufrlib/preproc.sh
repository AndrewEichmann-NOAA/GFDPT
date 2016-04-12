#!/bin/bash

# BUFRLIB version is BUFRLIB_v10-2-3.tar

export FC=/usr/local/intel/composerxe-2011.2.137/bin/intel64/ifort
export CC=/usr/bin/gcc

rm *.o 
rm libbufr.a
$FC -c *.f
$CC -c -DUNDERSCORE *.c
ar cru libbufr.a *.o

