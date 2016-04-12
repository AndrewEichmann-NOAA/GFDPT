PRO CHECK_COASTLINE, sceneData, refRadObs, dfov

;---find non-fill data 
idx = WHERE(refRadObs.lat ge -90,nidx)

;---write lat/lon observation locations to file 
scenefile = 'coast_test.dat'
OPENW, lun, scenefile, /GET_LUN
PRINTF, lun, nidx
PRINTF, lun, dfov
FOR i = 0L, nidx-1L DO BEGIN
  PRINTF, lun, refRadObs.lat[idx[i]], refRadObs.lon[idx[i]]
ENDFOR
FREE_LUN, lun
CLOSE, lun
;---run coast line check on observations
outfile = 'coast.out'
exe = 'bin/coast '
cmd = exe + ' > ' + outfile
SPAWN, cmd 

;---read output file
OPENR, lun, outfile
nprf = 1L
READF, lun, nprf
IF (nprf ne nidx) THEN stop
ix2a = LONARR(2)
ix2b = LONARR(2)
rx2  = FLTARR(2)
alat = FLTARR(nprf)
alon = FLTARR(nprf)
isfc = LONARR(nprf)
FOR ip = 0L, nprf-1L DO BEGIN
   READF, lun, ix2a, rx2, ix2b
   alat[ip] = rx2[0]
   alon[ip] = rx2[1]
   isfc[ip] = ix2b[1]
ENDFOR
CLOSE, lun

;---only mark cases where colocNWP said ocean
;   ---use DEM --- should be more strictly ocean
sfctyp = sceneData.sfcTypeVec[idx]
ii = WHERE(sfctyp eq 0,nii)
IF (nii eq 0) THEN RETURN 
sceneData.sfcTypeVec[idx[ii]] = isfc[ii]
;sceneData.sfcTypeVec[idx] = isfc
;---also mark frozen ocean via surface temperature
idx = WHERE(sceneData.sfcTypeVec eq 0 and $
            sceneData.TSkinVec lt 272.0,nidx)
sceneData.sfcTypeVec[idx] = 1  ; mark non-ocean

END
