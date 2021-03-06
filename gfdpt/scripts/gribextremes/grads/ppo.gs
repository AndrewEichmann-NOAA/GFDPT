*'open /tmp/wd23sm/rungras3w/flx.ctl'
'open /tmp/wd23sm/rungras32/flx.ctl'
*'open /tmp/wd23sm/jan3/flx.ctl'
*
'run /wd5/wd51/wd51js/rgbset.gs'
'set display color white'
'clear'
*
*'enable print outppoe.gr'
*'enable print Fig14.gr'
'enable print Fig15.gr'
*
'set t 1'
'set grads off'
'set grid off'
*'define pm3=ave(prate*86400,t=35,t=97)'
*'define pw3=ave(pwat,t=35,t=97)'
*'define ol3=ave(ulwrf,t=35,t=97)'
*'define em3=ave(lhtfl*0.03456,t=35,t=97)'
*
'define pm3=ave(prate*86400,t=53,t=115)'
'define pw3=ave(pwat,t=53,t=115)'
'define ol3=ave(ulwrf,t=53,t=115)'
'define em3=ave(lhtfl*0.03456,t=53,t=115)'
*
' set lon 0 360'
' set lat -90 90'
*
*  Plotting total Precipitation
*
'set parea 1.0 5.5  4.5 7.5'
'set grads off'
'set gxout shaded'
*'set clevs 4 8 16 32'
*'set ccols 0 23 25 27 29' 
'set clevs 2 4 8 16'
'set ccols 0 9 14 10 2'
*'set ccols 0 29 27 25 23' 
'd pm3'
'set gxout contour'
'set clevs 1 2 4 8 16 32'
'set clab off'
'set ccolor 1'
'set grads off'
'd pm3'
'set clab on'
'set clevs 1 2 4'
'set ccolor 1'
'set grads off'
*'d pm3'
*
'draw ylab Latitude'
'set string 1 tl 4 0'
'set strsiz 0.1'
'draw string 1.0 7.60 (a)'
'set string 1 tr 4 0'
'set strsiz 0.1'
'draw string 5.5 7.60 Rk=3'
'set string 1 tc 5 0'
'set strsiz 0.15'
'draw string 3.25 7.60 Precip. Rate (mm/day)'
*
'set parea 6.0 10.5  4.5 7.5'
*
*
*  Plotting total Precipitable Water
*
'set parea 6.0 10.5  4.5 7.5'
*
'set grads off'
'set gxout shaded'
*'set clevs 45 50 55 60'
*'set ccols 0 23 25 27 29' 
'set clevs 40 45 50 55'
'set ccols 0 9 14 10 2'
*'set ccols 0 29 27 25 23' 
'd pw3'
'set gxout contour'
'set clab off'
'set cint 5'
'set ccolor 1'
'set grads off'
'd pw3'
'set clab on'
'set clevs 5, 15, 25, 35 45'
'set ccolor 1'
'set grads off'
*'d pw3'
*
'set strsiz 0.1'
'set string 1 tl 4 0'
'draw string 6.0 7.60 (b)'
'set string 1 tr 4 0'
'draw string 10.5 7.60 Rk=3'
'set string 1 tc 5 0'
'set strsiz 0.15'
'draw string 8.25 7.60 PWAT (Kg/m**2)'
*
'set parea 1.0 5.5  1.0 4.0'
*
*  Plotting OLR 
*
'set grads off'
'set gxout shaded'
*'set clevs 160, 180, 200, 220 240'
*'set ccols 29 27 25 23 21 0' 
'set clevs 180, 200, 220 240 260'
'set ccols 2 10 5 14 9 0'
*'set ccols 21 23 25 27 29 0' 
'd ol3'
'set gxout contour'
'set cint 20'
'set clab off'
'set ccolor 1'
'set grads off'
'd ol3'
'set clab on'
'set clevs 220, 260, 300'
'set ccolor 1'
'set grads off'
*'d ol3'
'draw ylab Latitude'
'draw xlab Longitude'
*
'set strsiz 0.1'
'set string 1 tl 4 0'
'draw string 1.0 4.10 (c)'
'set string 1 tr 4 0'
'draw string 5.5 4.10 Rk=3'
'set string 1 tc 5 0'
'set strsiz 0.15'
'draw string 3.25 4.10 OLR (Watts/m**2)'
*
*  Plotting Evaporation 
*
'set parea 6.0 10.5  1.0 4.0'
*
'set grads off'
'set gxout shaded'
*'set clevs 4, 6, 8, 10'
*'set ccols 0 23 25 27 29' 
'set clevs 2 4 6 8'
'set ccols 0 9 14 10 2'
*'set ccols 0 29 27 25 23' 
'd em3'
'set gxout contour'
'set clab off'
'set cint 2'
'set ccolor 1'
'set grads off'
'd em3'
'draw xlab Longitude'
*
'set strsiz 0.1'
'set string 1 tl 4 0'
'draw string 6.0 4.10 (d)'
'set string 1 tr 4 0'
'draw string 10.5 4.10 Rk=3'
'set string 1 tc 5 0'
'set strsiz 0.15'
'draw string 8.25 4.10 EVP (mm/day)'
'set strsiz 0.2'
'set string 1 tc 6 0'
*'draw string 5.5 8.0 January Mean : Alf=0.2, Rk=3'
'draw string 5.5 8.0 July Mean : Alf=0.2, Rk=3'
*
'print'
