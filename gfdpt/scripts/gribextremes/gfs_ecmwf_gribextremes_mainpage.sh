#!/bin/sh
set -xa
date

#####################################################################
echo "------------------------------------------------"
echo "gfs_ecmwf_gribextremes_mainpage.sh"
echo "------------------------------------------------"
echo "V. Krishna Kumar Systems Integration Branch"
echo "History: APRIL 2011 - First implementation of this new script."
#
# Script to plot GRIBEXTREMES (GFS-ECMWF) over a geographical region in gempak
#
# Now set up GEMPAK/NTRANS environment
#
. /nwprod/gempak/.gempak
msg='gfs_ecmwf_gribextremes_mainpage.sh has begun'

#########################################
### Parameters and their range 
### obsc   -99999.        99999.       .1
#########################################

export gfile=$1
export hhmm=${cyc}'00'

cp $EXECPREP/sffdsf .
cp $FIXgempak/sffdsf.nts .
cp $FIXgempak/sffdsf.pdf .
cp $FIXgempak/sffdsf.prm .
cp $FIXgempak/sffdsf.err .
cp $FIXgempak/coltbl.xwp.wbg coltbl.xwp
cp $FIXgempak/plot_bias4.pack .
cp $FIXgempak/legendn.txt .

################################################
# Convert asci file to a gempak sfc file format 
# with the conversion program sffdsf 
################################################

#
#  Produce namelist file for the gempak sffdsf.
#  npgiv is the number of parameters to be plotted.
#  The first parameter is monthly counts
#
# nparm is the number of parameters for plotting
# Ensure that you are modifying the packing file 
# "*.pack" and the "SFPARM" and
# "COLORS" in the GEMPAK sfmap plotting script 
# to reflect the exact number of parameters
#
#

cat <<\gemEOF >gem_namlst
 &INPARM npgiv=4/
gemEOF


files=`ls ${gfile}`
for file in $files
do

gemgif=${gfile}.gif

####
#### Conversion gempak program from asci data to gempak extension surface
####
export pgm;. prep_step
export FORT9="gem_namlst"


sffdsf << EOF > sffdsf.record
 SFEFIL   = $file
 SFOUTF   = $file.sfc
 AREA     = -90;-180;90;180
 SFPRMF   = plot_bias4.pack
 DATTIM   = $gcpdy/$hhmm
 TIMSTN   = 1/3999
 FORMAT   = mklein
r

EOF

gpmap << EOF >> gpmap.record
 MAP      = 1
 MSCALE   = 0
 AREA     = dset
 GAREA    = world
 PROJ     = ced
 SATFIL   =
 RADFIL   =
 IMCBAR   =
 LATLON   = 
 PANEL    = 0
 TITLE    = 1
 TEXT     = 1
 CLEAR    = NO
 DEVICE   = gif|$gemgif | 1024;768
 LUTFIL   =
 STNPLT   =
 VGFILE   =
 AFOSFL   =
 AWPSFL   =
 LINE     = 3
 WATCH    =
 WARN     =
 HRCN     =
 ISIG     =
 LTNG     =
 ATCF     =
 AIRM     =
 GAIRM    =
 NCON     =
 CSIG     =
 SVRL     =
 BND      = bg/25 + world_bnds/10
 TCMG     =
 QSCT     =
 WSTM     =
 WOU      =
 WCN      =
 WCP      =
 ENCY     =
 FFA      =
 WSAT     =
 ASCT     =
 TRAK1    =
 TRAKE    =
 TRAK2    =
r

EOF

sfmap << EOF >> sfmap.record
 AREA     = dset
! GAREA    = -90;0;90;360
 GAREA    = world
 SATFIL   =
 RADFIL   =
 SFPARM   = mark:15:5:5:3;stid;;amdb;;airb;sonb;acab
! COLORS   = 23;(1;2;3;4/0;2;14;29;4/sonb/u);0;0;0;0
! COLORS   = 23;(1;2;3;4/0;2;8;29;4/sonb/u);0;0;0;0
 COLORS   = 23;(1;2;3;4/0;2;1;29;4/sonb/u);0;0;0;0
! wide pattern follows
!SFPARM   = mark:15:5:5:3;stid;;;;;;;;;;;;;;;;acab;;;amdb;airb;;;sonb
 DATTIM   = $gcpdy/$hhmm
 SFFILE   = $file.sfc
! MAP      = 1/1/1
 MAP      = 0
! LATLON   = 1/20/1/1/20;20
 TITLE    = 1/-2/Loc of Extreme GFS Analysis Difs ${cyc}Z $cpdy
 CLEAR    = no
 PANEL    = 0/1/1/2/view
 DEVICE   = gif|$gemgif | 1024;768
!PROJ     = mer
 PROJ     = ced
 FILTER   = no
! TEXT     = 1.00/22//11/s/sw/c--
 TEXT     = 1.25/3/2/7/s/sw/c--
!TEXT     = 1.25/3//7/s/sw/c--
 LUTFIL   =
 STNPLT   =
 CLRBAR   =
r

 TITLE    = 3/-1/DOC/NOAA/NWS/NCEP/NCO
r

EOF

gptext << EOF > gptext.record
PANEL    = 0/1/1/2/view
COLORS   = 4
TEXT     = 1.25/3/1/7/sw
CLEAR    = no
DEVICE   = gif|$gemgif | 1024;768
r

EOF

gpend

done

cp $gribout/*.gif $GIFOUT/.
cp $gribout/gemout.$PDY$cyc $GIFOUT/.
cp $gribout/gradsout.$PDY$cyc $GIFOUT/.

exit
