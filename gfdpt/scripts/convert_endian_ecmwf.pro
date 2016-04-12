Pro convert_endian_ecmwf, filein, fileout, npram

NLON=1440
NLAT=721

tmp1=fltarr(NLON,NLAT)

print, filein + ' ----> ' + fileout

openr, lun1, filein,  /get_lun, /swap_endian
openw, lun2, fileout, /get_lun

for i = 1, npram do begin
  readu,  lun1, tmp1
  writeu, lun2, tmp1
endfor

free_lun, lun1
free_lun, lun2

End
