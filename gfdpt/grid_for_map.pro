PRO GRID_FOR_MAP, radobs, radsim, lon, lat, MIN_LON, MAX_LON, MIN_LAT, MAX_LAT, dlatlon, $
                  rgridobs, rgridsim, latgrid, longrid, filter_all

;---------------------------------------------------------------------------------
; Name:  grid_for_map.pro
;
; Type:  IDL Program
;
; Description:
;   A local procedure to grid satellite measurement data at a desired
;   resolution. Input data should be filtered for valid values.
; 
;
; Author: Eric S. Maddy (RTi) @ JCSDA,
;         Eric.Maddy@noaa.gov
; Version: June 18, 2014, ESM, initial version 
;
;---------------------------------------------------------------------------------

  ;--- number of latitude/longitude points in grid
  nlat = LONG( (MAX_LAT - MIN_LAT) / dlatlon + 1 )
  nlon = LONG( (MAX_LON - MIN_LON) / dlatlon + 1 )

  ;--- index of the input data locations
  ilt  = LONG( (lat - MIN_LAT)/dlatlon + 0.49 )
  iln  = LONG( (lon - MIN_LON)/dlatlon + 0.49 )
  
  ;--- keep the indices within ranges
  idx = WHERE(ilt ge nlat,ndx)
  IF (ndx ne 0) THEN ilt[idx] = nlat-1
  idx = WHERE(ilt lt 0,ndx)
  IF (ndx ne 0) THEN ilt[idx] = 0
  idx = WHERE(iln ge nlon,ndx)
  IF (ndx ne 0) THEN iln[idx] = nlon-1
  idx = WHERE(ilt lt 0,ndx)
  IF (ndx ne 0) THEN iln[idx] = 0

  npts = N_ELEMENTS(lat)
;  i1   = SIZE(radobs,/DIMENSION) 
;  npts = i1[0]
  
  rundef   = -999.
  rgridobs = FLTARR(nlon,nlat) 
  rgridsim = FLTARR(nlon,nlat) 
  latgrid  = FLTARR(nlon,nlat) 
  longrid  = FLTARR(nlon,nlat) 
  ngridobs = LONARR(nlon,nlat)

  ;--- loop over the input data and accumulate statistics
  FOR ipts = 0L, npts-1L DO BEGIN

     ix = iln[ipts] &  iy = ilt[ipts]
     robs = radobs[ipts]
     rsim = radsim[ipts]
     rgridobs[ix,iy] += robs
     rgridsim[ix,iy] += rsim
     latgrid[ix,iy]  += lat[ipts]
     longrid[ix,iy]  += lon[ipts]
     ngridobs[ix,iy] += 1

  ENDFOR

  ;--- average input data on grid locations
  rgridobs = REFORM(rgridobs/FLOAT(ngridobs),[nlat*nlon,1])
  rgridsim = REFORM(rgridsim/FLOAT(ngridobs),[nlat*nlon,1])

  latgrid = REFORM(latgrid/FLOAT(ngridobs),[nlat*nlon,1])
  longrid = REFORM(longrid/FLOAT(ngridobs),[nlat*nlon,1])

  ;--- return filter of valid data locations
  filter_all = WHERE(FINITE(latgrid) eq 1, n_all)
  
  
END
