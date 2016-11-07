#!/bin/sh
set -x

#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#  1. VSDB stats are computed from anomalies relative to CDAS 30-year means (/nwprod/fix/cmean_1d.x)
#  2. Make plots that compare scores from one model for multiple cycles (e.g. 00Z 06Z 12Z 18Z)
#  Fanglin Yang
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------

## -- verification dates
CDATE=$(date +%Y%m%d)
edate=${1:-$CDATE}      ;#end of verification date
ndays=${2:-31}          ;#number of days back for verification
fdays=${3:-10}          ;#forecast length in days to be verified 


## -- verification parameters
export vtype=${vtype:-anom}
export cyclist=${cyclist:-"00 06 12 18"}
export vnamlist=${vnamlist:-"HGT"}
export mdlist=${mdlist:-"gfs"}
export levlist=${levlist:-"P500"}
export reglist=${reglist:-"G2/NHX G2/SHX"}

export webhost=${webhost:-"lnx321.ncep.noaa.gov"}
export webhostid=${webhostid:-"sdm"}
export vsdb_data=${vsdb_data:-/nco/$grp/f-f-divergence/vsdb_data}
export scriptsdir=${scriptsdir:-/nco/$grp/f-f-divergence/scripts}
export ftpdir=${ftpdir:-/home/nco/sdm/sdm/fcstdiv/main} 
export doftp=${doftp:-"YES"}
export archmon=${archmon:-NO}  ;# archive monthly means 
export rundir=${rundir:-/stmp/$LOGNAME/f-f-divergence/vsdb_stats}

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
for model in $mdlist ; do
for vnam in $vnamlist; do
   vnam1=`echo $vnam | sed "s?/??g" |sed "s?_WV1?WV?g"`
   export exedir=${rundir}/${vtype}/${model}/${vnam1}
   if [ -s $exedir ]; then rm -r $exedir; fi
   mkdir -p $exedir; cd $exedir || exit

for lev  in $levlist ; do
for reg  in $reglist ; do
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------

# -- generate output names 
  nhours=`expr $ndays \* 24 - 24`
  tmp=`/nwprod/util/exec/ndate -$nhours ${edate}00 `
  sdate=`echo $tmp | cut -c 1-8`
  reg1=`echo $reg | sed "s?/??g"`
  outname1=${model}_${vnam1}_${lev}_${reg1}_${sdate}${edate}
  yyyymm=`echo $edate |cut -c 1-6`
  outmon=${model}_${vnam1}_${lev}_${reg1}_${yyyymm}

# -- search data for all models; write out binary data, create grads control file
  for cyc  in $cyclist ; do
     outname=${outname1}_${cyc}
     if [ $vnam = "WIND" ]; then
      $scriptsdir/gen_wind.sh $vtype $model $vnam $reg $lev $edate $ndays $cyc $fdays $outname
     else
      $scriptsdir/gen_scal.sh $vtype $model $vnam $reg $lev $edate $ndays $cyc $fdays $outname
     fi
  done


#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# -- create grads scripts, allows up to 8 cycles 
nmd=`echo $cyclist | wc -w`  ;#count number of cycles
set -A mdname $cyclist
#set -A mdnamec 00Z 06Z 12Z 18Z 
set -A mdnamec 00Z 12Z 
modelc=`echo $model |tr "[a-z]" "[A-Z]" `
namedaily=${model}_${vnam1}_${lev}_${reg1}


# ----- PLOT TYPE 1:  time series of forecast-forecast correlations ----
cat >acz_${outname1}.gs <<EOF1 
'reinit'; 'set font 1'
              'open ${outname1}_${mdname[0]}.ctl'; mdc.1=${mdnamec[0]}
if($nmd >1); 'open ${outname1}_${mdname[1]}.ctl' ; mdc.2=${mdnamec[1]} ;endif
if($nmd >2); 'open ${outname1}_${mdname[2]}.ctl' ; mdc.3=${mdnamec[2]} ;endif
if($nmd >3); 'open ${outname1}_${mdname[3]}.ctl' ; mdc.4=${mdnamec[3]} ;endif
if($nmd >4); 'open ${outname1}_${mdname[4]}.ctl' ; mdc.5=${mdnamec[4]} ;endif
if($nmd >5); 'open ${outname1}_${mdname[5]}.ctl' ; mdc.6=${mdnamec[5]} ;endif
if($nmd >6); 'open ${outname1}_${mdname[6]}.ctl' ; mdc.7=${mdnamec[6]} ;endif
if($nmd >7); 'open ${outname1}_${mdname[7]}.ctl' ; mdc.8=${mdnamec[7]} ;endif

*-- define line styles and model names 
  cst.1=1; cst.2=5; cst.3=3; cst.4=5; cst.5=5; cst.6=3; cst.7=5; cst.8=5
  cth.1=9; cth.2=9; cth.3=4; cth.4=1; cth.5=4; cth.6=1; cth.7=1; cth.8=1
  cma.1=2; cma.2=8; cma.3=6; cma.4=1; cma.5=2; cma.6=0; cma.7=3; cma.8=7
  cco.1=1; cco.2=2; cco.3=3; cco.4=4; cco.5=8; cco.6=9; cco.7=5; cco.8=6
*------------------------
day=0 ;*start from fcst00
while ( day <= ${fdays} )
*------------------------
  'c'
  'set t 1 $ndays' 
   laty=day+1
  'set y '%laty 

  xwd=7.0; ywd=4.0; yy=ywd/13
  xmin=1.0; xmax=xmin+xwd; ymin=6.0; ymax=ymin+ywd
  xt=xmin+0.3; xt1=xt+0.5; xt2=xt1+0.1; yt=ymin+0.5*ywd ;*for legend  
  titlx=xmin+0.5*xwd;  titly=ymax+0.20                  ;*for tytle
  xlabx=xmin+0.5*xwd;  xlaby=ymin-0.60
  xcrmin=xmin;xcrmax=xmax;ycrmin=yt+0.29;ycrmax=ycrmin
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

*--find maximum and minmum values
   cmax=-1.0; cmin=1.0
   i=1
*  while (i <= $nmd)
   while (i <= 1)
    'set gxout stat'
    'd cor.'%i
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > cmax); cmax=zmax; endif
    if(zmin < cmin); cmin=zmin; endif
   i=i+1
   endwhile
   dist=cmax-cmin; cmin=cmin-0.9*dist; cmax=1.2*cmax
   if (cmin < -1.0); cmin=-1.0; endif
   if (cmax > 1.0); cmax=1.0; endif
   cmin=substr(cmin,1,3); cmax=substr(cmax,1,3); cint=0.1
   say 'cmin cmax cint 'cmin' 'cmax' 'cint

   i=1
   while (i <= $nmd)
    'set gxout stat'  ;* first compute means and count good data numbers
    'd cor.'%i
    ln=sublin(result,11); wd=subwrd(ln,2); a=substr(wd,1,5)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if ( b>0 )
      'set strsiz 0.13 0.13'; yt=yt-yy
      'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
      'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i'  ' a'  'b

      if ( day=5 )
      if ( $vnam = HGT )
      if ( $lev = P500 )
      if ( $modelc = PRA )
      if ( $reg1 = G2NHX | $reg1 = G2SHX )
       'set gxout line'
        cmin=0.3
       'set line 'cco.1' 'cst.1' 11'; 'draw line 'xcrmin' 'ycrmin' 'xcrmax' 'ycrmax
       if( i=1 )
       'set t 36'
       endif
       if( i=2 )
       'set t 35'
       endif
       'run $scriptsdir/f-f-divergence-fprintf.gs cor.'%i' cor.day'%day'.'%i'.${vnam}.${lev}.${reg1}.${modelc}.txt'
*      
* Call the SDM e-mail alert script here after generating the correlation text files for
*   
* sh $scriptsdir/vsdb_f-f_dalert.sh
*   
*  
* 
      endif
      endif
      endif
      endif
      endif

      'set gxout line'
      'set t 1 36'
      'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 'cmin' 'cmax; 'set ylint 'cint; 'set xlint 5'
**    'set vrange 0.2 1';        'set ylint 0.1' 'set xlint 5'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'd cor.'%i
     endif
   i=i+1
   endwhile

  'set string 1 bc 7'
  'set strsiz 0.16 0.16'
  'draw string 'titlx' 'titly' F-F Correl: ${vnam} ${lev} ${reg} ${modelc}, Day '%day
  'set strsiz 0.15 0.15'
  'draw string 'xlabx' 'xlaby' Verification Date'

  'printim cor_day'%day'_${namedaily}.png x800 y800'
  'set vpage off'
*--------
day=day+1
endwhile
*-------
'quit'
EOF1
grads -bcp "run acz_${outname1}.gs"
###
### Call the SDM e-mail alert script here after generating the correlation text files for
###
### MOVED TO END OF SCRIPT
#sh $scriptsdir/vsdb_f-f_dalert.sh
###
###
###
# ----- PLOT TYPE 2:  Die-off plot for mean correlation over $ndays days----
ndaysp1=`expr $ndays + 1`
fdaysp1=`expr $fdays + 1`
cat >cordieoff_${outname1}.gs <<EOF1 
'reinit'; 'set font 1'
              'open ${outname1}_${mdname[0]}.ctl'; mdc.1=${mdnamec[0]}
if($nmd >1); 'open ${outname1}_${mdname[1]}.ctl' ; mdc.2=${mdnamec[1]} ;endif
if($nmd >2); 'open ${outname1}_${mdname[2]}.ctl' ; mdc.3=${mdnamec[2]} ;endif
if($nmd >3); 'open ${outname1}_${mdname[3]}.ctl' ; mdc.4=${mdnamec[3]} ;endif
if($nmd >4); 'open ${outname1}_${mdname[4]}.ctl' ; mdc.5=${mdnamec[4]} ;endif
if($nmd >5); 'open ${outname1}_${mdname[5]}.ctl' ; mdc.6=${mdnamec[5]} ;endif
if($nmd >6); 'open ${outname1}_${mdname[6]}.ctl' ; mdc.7=${mdnamec[6]} ;endif
if($nmd >7); 'open ${outname1}_${mdname[7]}.ctl' ; mdc.8=${mdnamec[7]} ;endif

*-- define line styles and model names 
  cst.1=1; cst.2=1; cst.3=1; cst.4=1; cst.5=1; cst.6=1; cst.7=1; cst.8=1
  cth.1=9; cth.2=9; cth.3=9; cth.4=5; cth.5=5; cth.6=5; cth.7=5; cth.8=5
  cma.1=0; cma.2=0; cma.3=0; cma.4=0; cma.5=0; cma.6=0; cma.7=0; cma.8=0
  cco.1=1; cco.2=2; cco.3=3; cco.4=4; cco.5=8; cco.6=9; cco.7=5; cco.8=6

  'set t $ndaysp1' 
  'set y 1 ${fdaysp1}'
  xwd=7.0; ywd=5.0; yy=ywd/15
  xmin=0.8; xmax=xmin+xwd; ymin=5.0; ymax=ymin+ywd
  xt=xmin+0.3; xt1=xt+0.5; xt2=xt1+0.1; yt=ymin+0.5*ywd ;*for legend  
  titlx=xmin+0.5*xwd;  titly=ymax+0.20                  ;*for tytle
  xlabx=xmin+0.5*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

   i=1
   while (i <= $nmd)
    'set gxout stat'  
    'd bincor.'%i     ;* number of records
    ln=sublin(result,11); wd=subwrd(ln,2); a=substr(wd,1,3)
    if ( a>0 )
      'set strsiz 0.13 0.13'; yt=yt-yy
      'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
      'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i' 'a

      'set gxout line'
      'set mproj off'
      'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 0.2 1.0'; 'set ylint 0.1'; 'set xlint 24'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'd cor.'%i
     endif
   i=i+1
   endwhile

  'set string 1 bc 6'
  'set strsiz 0.13 0.13'
  'draw string 'titlx' 'titly' F-F Correlation: ${vnam} ${lev} ${reg} ${modelc}, $sdate-$edate '
  'set strsiz 0.14 0.14'
  'draw string 'xlabx' 'xlaby' Forecast Hour'

  'printim cordieoff_${namedaily}.png x800 y800'
  'set vpage off'
'quit'
EOF1
grads -bcp "run cordieoff_${outname1}.gs"


# ----- PLOT TYPE 3:  difference of F-F Correlation, other models minus first model  ----
cat >cordiff_${outname1}.gs <<EOF1 
'reinit'; 'set font 1'
'run $scriptsdir/f-f-divergence-white.gs'
              'open ${outname1}_${mdname[0]}.ctl'; mdc.1=${mdnamec[0]}
if($nmd >1); 'open ${outname1}_${mdname[1]}.ctl' ; mdc.2=${mdnamec[1]} ;endif
if($nmd >2); 'open ${outname1}_${mdname[2]}.ctl' ; mdc.3=${mdnamec[2]} ;endif
if($nmd >3); 'open ${outname1}_${mdname[3]}.ctl' ; mdc.4=${mdnamec[3]} ;endif
if($nmd >4); 'open ${outname1}_${mdname[4]}.ctl' ; mdc.5=${mdnamec[4]} ;endif
if($nmd >5); 'open ${outname1}_${mdname[5]}.ctl' ; mdc.6=${mdnamec[5]} ;endif
if($nmd >6); 'open ${outname1}_${mdname[6]}.ctl' ; mdc.7=${mdnamec[6]} ;endif
if($nmd >7); 'open ${outname1}_${mdname[7]}.ctl' ; mdc.8=${mdnamec[7]} ;endif

  'set t 1 $ndays' 
  'set y 1 $fdaysp1' 
  nframe=$nmd
  nframe2=2
  nframe3=4
  xmin0=1.2;  xlen=3.0;  xgap=0.2
  ymax0=10.0; ylen=-2.8;  ygap=-0.1
  i=1
  while ( i <= nframe )
    icx=1; if (i > nframe2); icx=2; endif
    if (i > nframe3); icx=3; endif
    if (i > nframe4); icx=4; endif
    xmin=xmin0+(icx-1)*(xlen+xgap)
    xmax=xmin+xlen
    icy=i; if (i > nframe2); icy=i-nframe2; endif
    if (i > nframe3); icy=i-nframe3; endif
    if (i > nframe4); icy=i-nframe4; endif
    ymax=ymax0+(icy-1)*(ylen+ygap)
    ymin=ymax+ylen
    titlx=xmin+0.15
    titly=ymax-0.3
    'set parea 'xmin' 'xmax' 'ymin' 'ymax

    'run $scriptsdir/f-f-divergence-rgbset.gs'
    'set xlopts 1 4 0.0'
    'set ylopts 1 4 0.0'
      if($nmd <2)
        if(i=$nmd);'set xlopts 1 6 0.10';endif
      endif
      if($nmd >2)
        if(i=2|i=$nmd);'set xlopts 1 6 0.10';endif
      endif
    if(i<=2);'set ylopts 1 4 0.10';endif
    'set clopts 1 4 0.08'
    'set grid on'
    'set mproj off'

    'set gxout shaded'
    'set grads off'
    'set clevs   -0.3 -0.2 -0.1 -0.05 -0.01 0 0.01 0.05 0.1 0.2 0.3'
    'set rbcols 39   37   36   35   34   32  62   64   65  66   67   69'
    if(i=1); 'set clevs    0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 0.95' ;endif
    if(i=1); 'set rbcols 31  33  35  37  39  42  43  45  47  49';endif
    'set xlevs 0 24 48 72 96 120 144 168 192 216 240 264 288 312 336 360 384'
    if(i=1);'d cor.'i ;endif
    if(i>1);'d cor.'i' - cor.1' ;endif
*
    'set gxout contour'
    'set grads off'
    'set ccolor 1'
    'set clevs   -0.3 -0.2 -0.1 -0.05 -0.01 0 0.01 0.05 0.1 0.2 0.3'
    if(i=1); 'set clevs   0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 0.95' ;endif
    'set clab forced'
    'set cstyle 1'
    if(i=1);'d cor.'%i ;endif
*   if(i>1);'d cor.'%i' - cor.1' ;endif

    'set string 1 bl 7'
    'set strsiz 0.14 0.14'
    if(i = 1); 'draw string 'titlx' 'titly' 'mdc.1; endif
    if(i > 1); 'draw string 'titlx' 'titly' 'mdc.i' - 'mdc.1; endif
  i=i+1
  endwhile

  'set string 1 bc 6'
  'set strsiz 0.15 0.15'
  'draw string 4.2 10.4 F-F Correlation: ${vnam} ${lev} ${reg} ${modelc}'
  'set string 1 bc 5'
  'set strsiz 0.13 0.13'
  'draw string 4.3 3.8 Forecast Hour'
  'set strsiz 0.15 0.15'
  'run $scriptsdir/f-f-divergence-cbarn.gs 0.95 0 4.1 3.40'

  'printim cordiff_${namedaily}.png x700 y700'
  'set vpage off'
'quit'
EOF1
grads -bcp "run cordiff_${outname1}.gs"


# ----- PLOT TYPE 4:  time series of RMS Errors----
cat >rms_${outname1}.gs <<EOF1 
'reinit'; 'set font 1'
              'open ${outname1}_${mdname[0]}.ctl'; mdc.1=${mdnamec[0]}
if($nmd >1); 'open ${outname1}_${mdname[1]}.ctl' ; mdc.2=${mdnamec[1]} ;endif
if($nmd >2); 'open ${outname1}_${mdname[2]}.ctl' ; mdc.3=${mdnamec[2]} ;endif
if($nmd >3); 'open ${outname1}_${mdname[3]}.ctl' ; mdc.4=${mdnamec[3]} ;endif
if($nmd >4); 'open ${outname1}_${mdname[4]}.ctl' ; mdc.5=${mdnamec[4]} ;endif
if($nmd >5); 'open ${outname1}_${mdname[5]}.ctl' ; mdc.6=${mdnamec[5]} ;endif
if($nmd >6); 'open ${outname1}_${mdname[6]}.ctl' ; mdc.7=${mdnamec[6]} ;endif
if($nmd >7); 'open ${outname1}_${mdname[7]}.ctl' ; mdc.8=${mdnamec[7]} ;endif

*-- define line styles and model names 
  cst.1=1; cst.2=5; cst.3=3; cst.4=5; cst.5=5; cst.6=3; cst.7=5; cst.8=5
  cth.1=9; cth.2=9; cth.3=4; cth.4=1; cth.5=4; cth.6=1; cth.7=1; cth.8=1
  cma.1=0; cma.2=8; cma.3=6; cma.4=1; cma.5=2; cma.6=0; cma.7=3; cma.8=7
  cco.1=1; cco.2=2; cco.3=3; cco.4=4; cco.5=8; cco.6=9; cco.7=5; cco.8=6

*------------------------
day=0 ;*start from fcst00
while ( day <= ${fdays} )
*------------------------
  'c'
  'set t 1 $ndays' 
   laty=day+1
  'set y '%laty 

  xwd=7.0; ywd=4.0; yy=ywd/15
  xmin=1.0; xmax=xmin+xwd; ymin=6.0; ymax=ymin+ywd
  xt=xmin+0.3; xt1=xt+0.5; xt2=xt1+0.1; yt=ymin+0.45*ywd ;*for legend  
  titlx=xmin+0.5*xwd;  titly=ymax+0.20                  ;*for tytle
  xlabx=xmin+0.5*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

*--find maximum and minmum values
   cmax=-10000000.0; cmin=10000000.0
   i=1
*  while (i <= $nmd)
   while (i <= 1)
    'set gxout stat'
    'd rms.'%i
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > cmax); cmax=zmax; endif
    if(zmin < cmin); cmin=zmin; endif
   i=i+1
   endwhile
   dist=cmax-cmin; cmin=cmin-1.7*dist; cmax=1.2*cmax
   cmin=substr(cmin,1,5); cmax=substr(cmax,1,5); cint=10*substr((cmax-cmin)/100,1,4)
   if (cint = 0); cint=substr((cmax-cmin)/10,1,4); endif
   if (cint = 0); cint=0.1*substr((cmax-cmin),1,4); endif
   if (cint = 0); cint=0.01*substr((cmax-cmin)*10,1,4); endif
   say 'cmin cmax cint 'cmin' 'cmax' 'cint

   i=1
   while (i <= $nmd)
    'set gxout stat'  ;* first compute means and count good data numbers
    'd rms.'%i
    ln=sublin(result,11); wd=subwrd(ln,2); a=substr(wd,1,5)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if ( b>0 )
      'set strsiz 0.13 0.13'; yt=yt-yy
      'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
      'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i'  ' a'  'b

      'set gxout line'
      'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 'cmin' 'cmax; 'set ylint 'cint
      'set xlint 5'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'd rms.'%i
     endif
   i=i+1
   endwhile

  'set string 1 bc 7'
  'set strsiz 0.16 0.16'
  'draw string 'titlx' 'titly' RMS Err: ${vnam} ${lev} ${reg} ${modelc}, Day '%day
  'set strsiz 0.15 0.15'
  'draw string 'xlabx' 'xlaby' Verification Date'

  'printim rms_day'%day'_${namedaily}.png x800 y800'
  'set vpage off'
*--------
day=day+1
endwhile
*-------
'quit'
EOF1
###VKK grads -bcp "run rms_${outname1}.gs"



# ----- PLOT TYPE 5:  mean rms error growth curve over $ndays days----
ndaysp1=`expr $ndays + 1`
fdaysp1=`expr $fdays + 1`
cat >rmsdieoff_${outname1}.gs <<EOF1 
'reinit'; 'set font 1'
              'open ${outname1}_${mdname[0]}.ctl'; mdc.1=${mdnamec[0]}
if($nmd >1); 'open ${outname1}_${mdname[1]}.ctl' ; mdc.2=${mdnamec[1]} ;endif
if($nmd >2); 'open ${outname1}_${mdname[2]}.ctl' ; mdc.3=${mdnamec[2]} ;endif
if($nmd >3); 'open ${outname1}_${mdname[3]}.ctl' ; mdc.4=${mdnamec[3]} ;endif
if($nmd >4); 'open ${outname1}_${mdname[4]}.ctl' ; mdc.5=${mdnamec[4]} ;endif
if($nmd >5); 'open ${outname1}_${mdname[5]}.ctl' ; mdc.6=${mdnamec[5]} ;endif
if($nmd >6); 'open ${outname1}_${mdname[6]}.ctl' ; mdc.7=${mdnamec[6]} ;endif
if($nmd >7); 'open ${outname1}_${mdname[7]}.ctl' ; mdc.8=${mdnamec[7]} ;endif

*-- define line styles and model names 
  cst.1=1; cst.2=1; cst.3=1; cst.4=1; cst.5=1; cst.6=1; cst.7=1; cst.8=1
  cth.1=9; cth.2=9; cth.3=9; cth.4=5; cth.5=5; cth.6=5; cth.7=5; cth.8=5
  cma.1=0; cma.2=0; cma.3=0; cma.4=0; cma.5=0; cma.6=0; cma.7=0; cma.8=0
  cco.1=1; cco.2=2; cco.3=3; cco.4=4; cco.5=8; cco.6=9; cco.7=5; cco.8=6

  'set t $ndaysp1' 
  'set y 1 ${fdaysp1}'
  xwd=7.0; ywd=5.0; yy=ywd/15
  xmin=0.8; xmax=xmin+xwd; ymin=5.0; ymax=ymin+ywd
  xt=xmin+0.3; xt1=xt+0.5; xt2=xt1+0.1; yt=ymax;*for legend  
  titlx=xmin+0.5*xwd;  titly=ymax+0.20                  ;*for tytle
  xlabx=xmin+0.5*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

*--find maximum and minmum values
   cmax=-10000000.0; cmin=10000000.0
   i=1
*  while (i <= $nmd)
   while (i <= 1)
    'set gxout stat'
    'd rms.'%i
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > cmax); cmax=zmax; endif
    if(zmin < cmin); cmin=zmin; endif
   i=i+1
   endwhile
   cmin=0; cmax=substr(cmax,1,5); cint=10*substr((cmax-cmin)/100,1,4)
   if (cint = 0); cint=substr((cmax-cmin)/10,1,4); endif
   if (cint = 0); cint=0.01*substr((cmax-cmin)*10,1,4); endif
   say 'cmin cmax cint 'cmin' 'cmax' 'cint


   i=1
   while (i <= $nmd)
    'set gxout stat'  ;* first compute means and count good data numbers
    'd bincor.'%i    ;* number of records
    ln=sublin(result,11); wd=subwrd(ln,2); a=substr(wd,1,3)
    if ( a>0 )
      'set strsiz 0.13 0.13'; yt=yt-yy
      'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
      'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i' 'a

      'set gxout line'
      'set mproj off'
      'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 'cmin ' 'cmax; 'set ylint 'cint; 'set xlint 24'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'd rms.'%i
     endif
   i=i+1
   endwhile

  'set string 1 bc 6'
  'set strsiz 0.13 0.13'
  'draw string 'titlx' 'titly' RMS Err: ${vnam} ${lev} ${reg} ${modelc}, $sdate-$edate '
  'set strsiz 0.14 0.14'
  'draw string 'xlabx' 'xlaby' Forecast Hour'

  'printim rmsdieoff_${namedaily}.png x800 y800'
  'set vpage off'
'quit'
EOF1
###VKKK grads -bcp "run rmsdieoff_${outname1}.gs"


# ----- PLOT TYPE 6:  frequency distribution of forecast-forecast correlations ----
ndayfq=$ndays
if [ $ndayfq -gt 20 ]; then ndayfq=20; fi
nday05=`expr $ndayfq \/ 2 `
cat >freq_${outname1}.gs <<EOF1 
'reinit'; 'set font 1'
              'open ${outname1}_${mdname[0]}.ctl'; mdc.1=${mdnamec[0]}
if($nmd >1); 'open ${outname1}_${mdname[1]}.ctl' ; mdc.2=${mdnamec[1]} ;endif
if($nmd >2); 'open ${outname1}_${mdname[2]}.ctl' ; mdc.3=${mdnamec[2]} ;endif
if($nmd >3); 'open ${outname1}_${mdname[3]}.ctl' ; mdc.4=${mdnamec[3]} ;endif
if($nmd >4); 'open ${outname1}_${mdname[4]}.ctl' ; mdc.5=${mdnamec[4]} ;endif
if($nmd >5); 'open ${outname1}_${mdname[5]}.ctl' ; mdc.6=${mdnamec[5]} ;endif
if($nmd >6); 'open ${outname1}_${mdname[6]}.ctl' ; mdc.7=${mdnamec[6]} ;endif
if($nmd >7); 'open ${outname1}_${mdname[7]}.ctl' ; mdc.8=${mdnamec[7]} ;endif

*-- define line styles and model names 
  cst.1=1; cst.2=1; cst.3=1; cst.4=1; cst.5=1; cst.6=1; cst.7=1; cst.8=1
  cth.1=9; cth.2=9; cth.3=9; cth.4=5; cth.5=5; cth.6=5; cth.7=5; cth.8=5
  cma.1=0; cma.2=0; cma.3=0; cma.4=0; cma.5=0; cma.6=0; cma.7=0; cma.8=0
  cco.1=1; cco.2=2; cco.3=3; cco.4=4; cco.5=8; cco.6=9; cco.7=5; cco.8=6
*------------------------
**day=0 ;*start from fcst00
**while ( day <= ${fdays} )
 day=4 ;*start from fcst00
 while ( day <= 6 )
*------------------------
  'c'
  laty=day+1
  'set y '%laty; 'set t $nday05 $ndayfq' 
  xwd=7.0; ywd=5.0; yy=ywd/15
  xmin=0.8; xmax=xmin+xwd; ymin=5.0; ymax=ymin+ywd
  xt=xmin+0.3; xt1=xt+0.5; xt2=xt1+0.1; yt=ymax;*for legend  
  titlx=xmin+0.5*xwd;  titly=ymax+0.20                  ;*for tytle
  xlabx=xmin+0.5*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

   i=1
   while (i <= $nmd)
    'set gxout stat'; 'd bincor.'%i    
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,5)
    if ( b>0 )
      'set strsiz 0.13 0.13'; yt=yt-yy
      'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
      'set string 'cco.i' bl 6';        'draw string 'xt2' 'yt' 'mdc.i

      'set gxout line'
      'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 0 60';        'set ylint 10'
      'set xaxis 0.475 0.975 0.1'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
*     'd 100*smth9(bincor.'%i')'
      'd 100*bincor.'%i
     endif
   i=i+1
   endwhile

  'set string 1 bc 7'
  'set strsiz 0.16 0.16'
  'draw string 'titlx' 'titly' F-F Corr Percent Freq: ${vnam} ${lev} ${reg} ${modelc}, Day '%day
  'set strsiz 0.15 0.15'
  'draw string 'xlabx' 'xlaby' F-F Correlation,   ${sdate}-${edate}'

  'printim freq_day'%day'_${namedaily}.png x800 y800'
  'set vpage off'
*--------
day=day+1
endwhile
*-------
'quit'
EOF1
###VKK grads -bcp "run freq_${outname1}.gs"


#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#---------------------------------------------------------
# remove whitesapce from.png files
  nn=0
  while [ $nn -le $fdays ]; do
   convert -crop 0.2x0.2 cor_day${nn}_${namedaily}.png cor_day${nn}_${namedaily}.png
   convert -crop 0.2x0.2 rms_day${nn}_${namedaily}.png rms_day${nn}_${namedaily}.png
   convert -crop 0.2x0.2 freq_day${nn}_${namedaily}.png freq_day${nn}_${namedaily}.png
   nn=`expr $nn + 1 `
  done
  convert -crop 0.2x0.2 cordiff_${namedaily}.png   cordiff_${namedaily}.png
  convert -crop 0.2x0.2 cordieoff_${namedaily}.png cordieoff_${namedaily}.png
  convert -crop 0.2x0.2 rmsdieoff_${namedaily}.png rmsdieoff_${namedaily}.png
#---------------------------------------------------------

chmod a+rw cor*.png
chmod a+rw rms*.png
chmod a+rw freq*.png
chmod a+rw err*.png

## -- for daily rotatigng display
cat << EOF >ftpin
  binary
  promt
  cd $/gfs4cyc/daily/cor/day0
    mput cor_day0_${namedaily}.png
  cd $/gfs4cyc/daily/cor/day1
    mput cor_day1_${namedaily}.png
  cd $/gfs4cyc/daily/cor/day2
    mput cor_day2_${namedaily}.png
  cd $/gfs4cyc/daily/cor/day3
    mput cor_day3_${namedaily}.png
  cd $/gfs4cyc/daily/cor/day4
    mput cor_day4_${namedaily}.png
  cd $/gfs4cyc/daily/cor/day5
    mput cor_day5_${namedaily}.png
    mput cor*day5*txt
  cd $/gfs4cyc/daily/cor/cordiff
    mput cordiff_${namedaily}.png
  cd $/gfs4cyc/daily/dieoff
    mput cordieoff_${namedaily}.png
  quit
EOF
if [ $doftp = "YES" ]; then sftp ${webhostid}@${webhost} <ftpin ; fi

## -- make monthly archive 
yyyy=`echo $edate |cut -c 1-4`
if [ $archmon = "YES" ]; then
  nn=0
  while [ $nn -le $fdays ]; do
   cp cor_day${nn}_${namedaily}.png  cor_day${nn}_${outmon}.png
   cp rms_day${nn}_${namedaily}.png  rms_day${nn}_${outmon}.png
   cp freq_day${nn}_${namedaily}.png freq_day${nn}_${outmon}.png
   nn=`expr $nn + 1 `
  done
   cp cordiff_${namedaily}.png cordiff_${outmon}.png
   cp cordieoff_${namedaily}.png cordieoff_${outmon}.png
   cp rmsdieoff_${namedaily}.png rmsdieoff_${outmon}.png
cat << EOF >ftpin
  binary
  promt
  cd $/gfs4cyc/arch_mon/cor
  mkdir $yyyy
  cd $yyyy
    mput cor_day*_${outmon}.png
    mput cordiff_${outmon}.png
    mput meancor*.txt
  cd $/gfs4cyc/arch_mon/rms
  mkdir $yyyy
  cd $yyyy
    mput rms_day*_${outmon}.png
    mput meanrms*.txt
  cd $/gfs4cyc/arch_mon/dieoff
  mkdir $yyyy
  cd $yyyy
    mput cordieoff_${outmon}.png
    mput rmsdieoff_${outmon}.png
  cd $/gfs4cyc/arch_mon/freq
  mkdir $yyyy
  cd $yyyy
    mput freq_day*_${outmon}.png
  quit
EOF
if [ $doftp = "YES" ]; then sftp ${webhostid}@${webhost} <ftpin ; fi
fi
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

done  ;# end reg
done  ;# end lev
done  ;# end vnam
done  ;# end model
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------

###
### Call the SDM e-mail alert script here after generating the correlation text files for
###
#sh $scriptsdir/vsdb_f-f_dalert.sh
###
###
###


exit

