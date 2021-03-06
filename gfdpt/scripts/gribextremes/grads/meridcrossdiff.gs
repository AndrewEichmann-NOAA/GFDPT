'open ../d3d.ctl'
'set display color white'
'set lev 1 30'
'set lon 230 300'
prompt 'Enter the latitude for a meridional cross-section: '
pull clat
'set lat 'clat
'define cnv=ave(cnvhrhbl*86400,t=1,t=7)'
'define lw=ave(lwhrhbl*86400,t=1,t=7)'
'define sw=ave(swhrhbl*86400,t=1,t=7)'
'define sha=ave(shahrhbl*86400,t=1,t=7)'
'define vdf=ave(vdfhrhbl*86400,t=1,t=7)'
'define lrg=ave(lrghrhbl*86400,t=1,t=7)'

'open /global/save/wx23dc/warmt382/7daymean/d3d.ctl'
'define cnv2=ave(cnvhrhbl.2,t=1,t=7)*86400'
'define lw2=ave(lwhrhbl.2,t=1,t=7)*86400'
'define sw2=ave(swhrhbl.2,t=1,t=7)*86400'
'define sha2=ave(shahrhbl.2,t=1,t=7)*86400'
'define vdf2=ave(vdfhrhbl.2,t=1,t=7)*86400'
'define lrg2=ave(lrghrhbl.2,t=1,t=7)*86400'

'colors.gs'
'set cint 1'
'set gxout shaded'
'set black -.1 .1' 
'set clevs -8 -7 -6 -5 -4 -3 -2 -1 -.5 -.1 .1 .5 1 2 3 4 5 6 7 8'
'set ccols 49 48 47 46 45 44 43 42 41 51 0 31 21 22 23 24 25 26 27 28 29'
'd cnv-cnv2'
'cbarn'
'set cint 1'
'set gxout contour'
'set black -.1 .1'
'd cnv-cnv2'
'draw title \\\F24 Meridional cross-section deep convective heating [K/day] (T126-T382) at 'clat%'N'
'printim meridcnv_crossf24diff.png x1000 y800'

'c'
'set cint 1'
'set gxout shaded'
'set black -.1 .1'
'set clevs -20 -18 -16 -14 -12 -10 -8 -6 -4 -2 -1 -.5 -.1 .1 .5 1 2 4 6 8 10'
'set ccols 49 48 47 46 45 44 43 42 41 51 52 53 54 0 21 22 23 24 25 26 27 28'
'set lev 1 6'
'd lw-lw2'
'cbarn'
'set cint 1'
'set black -.1 .1'
'set gxout contour'
'd lw-lw2'
'draw title \\\F24 Meridional cross-section longwave radiative heating [K/day] (T126-T382) at 'clat%'N'
'printim meridlwave_crossf24diff.png x1000 y800'
'set lev 1 30'
'c'
'set cint 1'
'set gxout shaded'
'set black -.1 .1'
'set clevs -.1 .1 .2 .4 .6. .8 1 2 4 6 8 10'
'set ccols 41 0 31 33 34 21 22 23 24 25 26 27 28'
'd sw-sw2'
'cbarn'
'set cint 1'
'set black -.1 .1'
'set gxout contour'
'd sw-sw2'
'draw title \\\F24 Meridional cross-section shortwave radiative heating [K/day] (T126-T382) at 'clat%'N'
'printim meridswave_crossf24diff.png x1000 y800'

'c'
'set cint 1'
'set black -.1 .1'
'set gxout shaded'
'set clevs -2 -1 -.5 -.1 .1 .5 1 2 3 4 5 6'
'set ccols 49 47 45 43 0 21 22 23 24 25 26 27 28'
'd sha-sha2'
'cbarn'
'set cint 1'
'set black -.1 .1'
'set gxout contour'
'd sha-sha2'
'draw title \\\F24 Meridional cross-section shallow convective heating [K/day] (T126-T382) at 'clat%'N'
'printim meridshall_crossf24diff.png x1000 y800'

'c'
'set cint 1'
'set black -.1 .1'
'set gxout shaded'
'set clevs -8 -7 -6 -5 -4 -3 -2 -1 -.5 -.1 .1 .5 1 3 6 9 12 18 25'
'set ccols 49 48 47 46 45 44 43 42 41 51 0 21 22 23 24 25 26 27 28 29'
'd vdf-vdf2'
'cbarn'
'set cint 5'
'set black -.1 .1'
'set gxout contour'
'd vdf-vdf2'
'draw title \\\F24 Meridional cross-section vertical diffusion heating [K/day] (T126-T382) at 'clat%'N'
'printim meridvdiff_crossf24diff.png x1000 y800'

'c'
'set cint 1'
'set black -.1 .1'
'set gxout shaded'
'set clevs -7 -6 -5 -4 -3 -2 -1 -.5 -.1 .1 .5 1 2 3 4 5 6 7'
'set ccols 49 48 47 46 45 44 43 42 41 0 31 21 22 23 24 25 26 27 28 29'
'd lrg-lrg2'
'cbarn'
'set cint 1'
'set black -.1 .1'
'set gxout contour'
'd lrg-lrg2'
'draw title \\\F24 Meridional cross-section large scale condensation heating [K/day] (T126-T382) at 'clat%'N'
'printim meridlrg_crossf24diff.png x1000 y800'
'quit'
