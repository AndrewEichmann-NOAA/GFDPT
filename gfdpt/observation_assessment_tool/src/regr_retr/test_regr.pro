@./regress.pro

@./io_regressAlgors.pro


;PRO test_regr

tb=fltarr(14,1)
sensor_id='amsr2'
sfctype=intarr(1)
angle=fltarr(1)
scanpos=intarr(1)
clw=fltarr(1)
lat=fltarr(1)

coeff_path='/home/pub/kgarrett/dat_tool/data/regr_coeffs/'

tb[*,0]=255.
angle[0]=53.
scanpos[0]=10
sfctype[0]=0
lat[0]=-45.

retrieve_clw,Tb,sfctype,coeff_path,sensor_id,angle,scanpos,lat,clw
print, 'cloud',clw[0]




END




