;+
;---------------------------------------------------------------------------------
; Name:  loadscenedump.pro
;
; Type:  IDL Program
;
; Description:
;   To read scene dump files .
;
; Author: Eric S. Maddy (RTI) @ JCSDA,
;         Eric.Maddy@noaa.gov
; Version: Jan 21, 2015, ESM, Added emissivity and analytic emissivity
;                             to dump
;          May 2, 2014,  ESM, Initial coding
;
;-------------------------------------------------------------------------------;-

PRO LOADSCENEDUMP, file, scenedump

; open file and get a logical unit
OPENR, lunx, file, /F77_UNFORMATTED, /SWAP_IF_LITTLE, /GET_LUN 
  
rx = 9999.
ix = 1L
nchan = 1L
; read the header to figure how many profiles are in the file
READU, lunx, rx, ix, nchan
print, nchan
; scene dump structure
scenedump = {versid: rx, nprof: ix, $
             tpwvec: FLTARR(ix), $
             gwpvec: FLTARR(ix), $
             clwvec: FLTARR(ix), $
             rwpvec: FLTARR(ix), $
             tskinvec: FLTARR(ix), $
             sfctypvec: LONARR(ix), $
             psurfvec: FLTARR(ix), $
             winduvec: FLTARR(ix),$
             windvvec: FLTARR(ix),$
             emissvec: FLTARR(ix,nchan),$
             emissanlvec: FLTARR(ix,nchan)}
rx5 = FLTARR(5)
rx3 = FLTARR(3)
rxc  = FLTARR(nchan)
rxc2 = FLTARR(nchan)

; loop over profiles 
FOR iprof = 0L, scenedump.nprof-1L DO BEGIN
;  READU, lunx, clw, rwp, gwp, tpw, tskin, isfc, sfcpres
  READU, lunx, rx5, ix, rx3
  scenedump.clwvec[iprof] = rx5[0]
  scenedump.rwpvec[iprof] = rx5[1]
  scenedump.gwpvec[iprof] = rx5[2]
  scenedump.tpwvec[iprof] = rx5[3]
  scenedump.tskinvec[iprof] = rx5[4]
  scenedump.sfctypvec[iprof] = ix
  scenedump.psurfvec[iprof] = rx3[0]
  scenedump.winduvec[iprof] = rx3[1]
  scenedump.windvvec[iprof] = rx3[2]
  ;---read emissivity from the file 
  READU, lunx, rxc, rxc2
  scenedump.emissvec[iprof,*] = rxc
  scenedump.emissanlvec[iprof,*] = rxc2
ENDFOR 

; close and free logical unit
CLOSE, lunx
FREE_LUN, lunx

END
