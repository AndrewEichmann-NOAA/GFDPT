'reinit'
'open /tmp/wd23sm/pgb/pgb.ctls1'
'enable print outs1.gr'
*
'run /wd5/wd51/wd51js/rgbset.gs'
'set display color white'
'clear'
*
'set t 1'
'set lev 1000 10'
'set grads off'
'set grid off'
'set lon 0 360'
'set lat -90 90'
'define fld=ave(ugrd,t=1,t=3)'
'define um=ave(fld,lon=0,lon=360,-b)'
'define fld=ave(vgrd,t=1,t=3)'
'define vm=ave(fld,lon=0,lon=360,-b)'
'define fld=ave(tmp,t=1,t=3)'
'define tm=ave(fld,lon=0,lon=360,-b)'
'define fld=ave(spfh,t=1,t=3)'
'define qm=ave(fld,lon=0,lon=360,-b)'
*
*
' set lon 0'
' set lat -90 90'
'set zlog on'
*
*  Plotting Zonal Wind
*
'set parea 1.0 5.5  4.5 7.5'
'set grads off'
'set gxout shaded'
'set clevs 30 50 70'
'set ccols 0 9 14 10' 
'd um'
'set gxout contour'
'set cint 5 '
'set clab off'
'set ccolor 1'
'set grads off'
'd um'
'set clab on'
'set clevs 10 20 40 60 80'
'set ccolor 1'
'set grads off'
'd um'
*
'draw ylab Pressure (hPa)'
'set string 1 tl 4 0'
'set strsiz 0.1'
'draw string 1.0 7.70 (a)'
'set string 1 tr 4 0'
'set strsiz 0.1'
*'draw string 5.5 7.70 Rk=3'
'set string 1 tc 5 0'
'set strsiz 0.15'
'draw string 3.25 7.70 Zonal wind (m/sec)'
*'run /emcsrc3/wd23sm/scripts/cbarnew.gs 0.7 0 3.25 4.25'
*
'set parea 6.0 10.5  4.5 7.5'
*
*
*  Plotting Meridional Wind
*
'set parea 6.0 10.5  4.5 7.5'
*
'set grads off'
'set gxout shaded'
'set clevs -3.0 -2.0 -1.0'
'set ccols 10 14 9 0' 
'd vm'
'set gxout contour'
'set clab off'
'set cint 0.5'
'set ccolor 1'
'set grads off'
'd vm'
'set clab on'
'set clevs 0.5, 1.0, 2.0, 3.0 4.0'
'set ccolor 1'
'set grads off'
'd vm'
*
'set strsiz 0.1'
'set string 1 tl 4 0'
'draw string 6.0 7.70 (b)'
'set string 1 tr 4 0'
*'draw string 10.5 7.70 Rk=3'
'set string 1 tc 5 0'
'set strsiz 0.15'
'draw string 8.25 7.70 Meridional Wind (m/sec)'
*'run /emcsrc3/wd23sm/scripts/cbarnew.gs 0.7 0 8.25 4.25'
*
'set parea 1.0 5.5  1.0 4.0'
*
*  Plotting Temperature 
*
'set grads off'
'set gxout shaded'
'set clevs 180, 200, 220'
'set ccols 5 14 9 0' 
'd tm'
'set gxout contour'
'set cint 5'
'set clab off'
'set ccolor 1'
'set grads off'
'd tm'
'set clab on'
'set clevs 200 220 240 260 280 '
'set ccolor 1'
'set grads off'
'd tm'
'draw ylab Pressure (hPa)'
'draw xlab Latitude'
*
'set strsiz 0.1'
'set string 1 tl 4 0'
'draw string 1.0 4.20 (c)'
'set string 1 tr 4 0'
*'draw string 5.5 4.20 Rk=3'
'set string 1 tc 5 0'
'set strsiz 0.15'
'draw string 3.25 4.20 Temperature (K)'
*'run /emcsrc3/wd23sm/scripts/cbarnew.gs 0.7 0 3.25 0.3'
*
*  Specific Humidity
*
'set parea 6.0 10.5  1.0 4.0'
*
'set lev 1000 100'
'set zlog off'
'set grads off'
'set gxout shaded'
'set clevs 8 10 12 14'
'set ccols 0 9 14 10' 
'd qm'
'set gxout contour'
'set clab off'
'set cint 1'
'set ccolor 1'
'set grads off'
'd qm'
'set clab on'
'set clevs 1 3 5 7 9 11 13 15'
'set ccolor 1'
'd qm'
'draw xlab Latitude'
*
'set strsiz 0.1'
'set string 1 tl 4 0'
'draw string 6.0 4.20 (d)'
'set string 1 tr 4 0'
*'draw string 10.5 4.20 Rk=3'
'set string 1 tc 5 0'
'set strsiz 0.15'
'draw string 8.25 4.20 Specific Humidity (mm/day)'
'set strsiz 0.2'
'set string 1 tc 6 0'
'draw string 5.5 8.0 JJA Mean : RAS V1'
*'run /emcsrc3/wd23sm/scripts/cbarnew.gs 0.7 0 8.25 0.3'
*
'print'
