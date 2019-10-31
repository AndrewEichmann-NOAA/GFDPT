'reinit'
'open /tmp/wd23sm/cr1/grb.ctl'
'open /tmp/wd23sm/cr2/grb.ctl'
'open /tmp/wd23sm/cr3/grb.ctl'
*
'run /wd5/wd51/wd51js/rgbset.gs'
'set display color white'
'clear'
*
'enable print outj.gr'
*
'set z 1 28'
'set lon 0 360'
'set lat -30 30'
*
'define fac=2500/1.0035'
'define tmp=ave(cnvhr*86400,t=4,t=6)'
'define tcu1=aave(tmp,lon=0,lon=360,lat=-20,lat=20)'
'define tmp=ave(cnvmr*86400,t=4,t=6)'
'define qcu1=aave(tmp*fac,lon=0,lon=360,lat=-20,lat=20)'
*'define tmp=ave(lrghr*86400,t=1,t=3)'
*'define lrg1=aave(tmp,lon=0,lon=360,lat=-30,lat=30)'
*
*
'define tmp=ave(cnvhr.2*86400,t=4,t=6)'
'define tcu2=aave(tmp,lon=0,lon=360,lat=-20,lat=20)'
'define tmp=ave(cnvmr.2*86400,t=4,t=6)'
'define qcu2=aave(tmp*fac,lon=0,lon=360,lat=-20,lat=20)'
*'define tmp=ave(lrghr.2*86400,t=1,t=3)'
*'define lrg2=aave(tmp,lon=0,lon=360,lat=-20,lat=20)'
*
*
'define tmp=ave(cnvhr.3*86400,t=4,t=6)'
'define tcu3=aave(tmp,lon=0,lon=360,lat=-20,lat=20)'
'define tmp=ave(cnvmr.3*86400,t=4,t=6)'
'define qcu3=aave(tmp*fac,lon=0,lon=360,lat=-20,lat=20)'
*'define tmp=ave(lrghr.3*86400,t=1,t=3)'
*'define lrg3=aave(tmp,lon=0,lon=360,lat=-20,lat=20)'
*
'set t 1'
'set lev 1000 10'
'set lon 0'
'set grads off'
'set grid off'
*
'set parea 1.0 5.5  1.5 7.5'
'set axlim -2 3'
'set cthick 7'
'set ccolor 1'
'set cstyle 1'
'set cmark 0'
'd tcu1'
*'d tcu1+lrg1'
'set cthick 4'
'set ccolor 1'
'set cmark 0'
'set cstyle 1'
'set grads off'
'd tcu2'
*'d tcu2+lrg2'
'set ccolor 1'
'set cmark 0'
'set cstyle 2'
'set grads off'
'd tcu3'
*'d tcu3+lrg3'
'draw title 20S-20N JJA'
'draw ylab Pressure (hPa)'
'draw xlab Convective Heating (K/Day)'
'set string 0 tr 4 0'
'set strsiz 0.2'
*'draw string 1.0 7.6 (a)'
'run /emcsrc3/wd23sm/scripts/lnspcc.gs leg3q1q2'
'set strsiz 0.15'
'draw string 1.0 7.65 (a)'
*
'set parea 6.0 10.5  1.5 7.5'
'set axlim -4 1'
'set cthick 7'
'set ccolor 1'
'set cmark 0'
'set grads off'
'set cstyle 1'
'd qcu1'
*'d qcu1-lrg1/2.5'
'set cthick 4'
'set ccolor 1'
'set cmark 0'
'set cstyle 1'
'set grads off'
'd qcu2'
*'d qcu2-lrg2/2.5'
'set ccolor 1'
'set cmark 0'
'set cstyle 2'
'set grads off'
'd qcu3'
*'d qcu3-lrg3/2.5'
*
'draw title 20S-20N JJA'
'draw xlab Convective Moistening (K/Day)'
'set strsiz 0.15'
'draw string 6.0 7.65 (b)'
'run /emcsrc3/wd23sm/scripts/lnspcc.gs leg3q1q2'
*
'print'