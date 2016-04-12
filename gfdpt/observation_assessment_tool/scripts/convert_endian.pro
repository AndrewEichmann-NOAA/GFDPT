Pro convert_endian, filein, fileout, npram

tmp1=fltarr(360,181)
tmp2=fltarr(181,360)
tmp3=fltarr(181,360)

openw, lun2, fileout, /get_lun
openr, lun1, filein,  /get_lun, /swap_endian

for i = 1, npram do begin
  readu,  lun1, tmp1
  tmp2 = TRANSPOSE(tmp1)
  for j=0,180 do tmp3(j,*) = tmp2(180-j,*)
  writeu, lun2, tmp3
endfor

close, lun1
free_lun, lun1

close, lun2
free_lun, lun2


End
