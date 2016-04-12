;---------------------------------------------------------------------------------
; Summary of all subroutines related to I/O processes for the
; different sceneamv files (geophysical data).
;
; E.S. Maddy Riverside Tech. Inc. @ NOAA/NESDIS 2014 
;
;---------------------------------------------------------------------------------

;===============================================================
; Name:		Def_SceneAMV
;
;
; Type:		IDL Subroutine
;
;
; Description:  Define SceneAMV type
;
;
; Arguments:
;
;	    Name	    Type	    Description
;      ---------------------------------------------------
;	- SceneAMV          O               SceneAMV structure
;       - nprofiles         I               number of scenes
;       - nQC               I               number of QC/QI parameters
; Subroutines needed:
;       - None
;
;
; History:
;
;===============================================================

PRO Def_SceneAMV, SceneAMV, nQC, nProfiles
 ix = LONARR(nProfiles) - 999L
 rx = FLTARR(nProfiles) - 999.9
 SceneAMV = {nProfiles         : nProfiles, $
             iTyp              : -999L, $
             AlgSN             : -999L, $
             nqc               : nQC, $
             AlgDesc           : STRARR(nProfiles), $
             ProfIndx          : ix, $
             HeightMethod      : ix, $
             WindCalcMethod    : ix, $
             angle             : rx, $
             WindSp            : rx, $
             WindDir           : rx, $
             WindU             : rx, $
             WindV             : rx, $
             WindPress         : rx, $
             WindError         : rx, $
             qc                : INTARR(nProfiles,nQC), $
             qi                : FLTARR(nProfiles,nQC), $
             lat               : rx, $
             lon               : rx, $
             node              : ix, $
             scanUTC           : rx, $
             scanyear          : ix, $
             scanmonth         : ix, $
             scanday           : ix, $
             iscanPos          : ix, $
             iscanLine         : ix}

END
;-------------------------------------------------------------------------------------
;
;
; Geophysical I/O subroutines
;
;
;-------------------------------------------------------------------------------------

;===============================================================
; Name:		WriteSceneamvHdr
;
;
; Type:		IDL Subroutine
;
;
; Description:  Writes the header of the sceneamv file with all
;               info coming from inputs
;
;
; Arguments:
;
;	    Name	    Type	    Description
;      ---------------------------------------------------
;	- iu                 O           Unit number       
;	- file               I           Name of file to write
;       - SceneAMV           I           Structure containing SceneAMV_type
;	- nProfiles          I           Number of profiles

;
; Subroutines needed:
;       - None
;
;
; History:
;
;===============================================================
PRO WriteSceneamvHdr,iu,file,SceneAMV,nProfiles
    ;----------------------------------------------------------------------------------
    ; Subroutine to write the Sceneamv file (either truth file or satellite-retrieved file).
    ;
    ;----------------------------------------------------------------------------------
    OPENW,iu,file,/get_lun,/f77_unformatted;,/swap_if_little_endian
    WRITEU, iu, SceneAMV.iTyp, SceneAMV.AlgSN
    WRITEU,iu, nProfiles
    WRITEU,iu, SceneAMV.nqc
END

;===============================================================
; Name:		ReadSceneAMVHdr
;
;
; Type:		IDL Subroutine
;
;
; Description:  Writes the header of the sceneamv file with all
;               info coming from inputs
;
;
; Arguments:
;
;	    Name	    Type	    Description
;      ---------------------------------------------------
;	- iu                 O           Unit number       
;	- file               I           Name of file to write
;       - SceneAMV          I/O           Structure containing SceneAMV_type

;
; Subroutines needed:
;       - None
;
;
; History:
;
;===============================================================
PRO ReadSceneAMVHdr,iu, file, SceneAMV
    ;----------------------------------------------------------------------------------
    ; Subroutine to write the Sceneamv file (either truth file or satellite-retrieved file).
    ;
    ;----------------------------------------------------------------------------------
    OPENR, iu, file, /GET_LUN, /F77_UNFORMATTED, /SWAP_IF_LITTLE_ENDIAN
    iType = 1L & AlgSN = 1L
    nProfiles = 1L
    nqc = 1L

    READU, iu, iTyp, AlgSN
    READU, iu, nProfiles
    READU, iu, nqc
    Def_SceneAMV, SceneAMV, nQC, nProfiles
    SceneAMV.nProfiles = nProfiles
    SceneAMV.nQC       = nQC
    SceneAMV.iTyp      = iTyp
    SceneAMV.AlgSN     = AlgSN
    
END

;===============================================================
; Name:		ReadSceneAMV
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads the sceneamv with all
;               info coming from inputs
;
;
; Arguments:
;
;	    Name	    Type	    Description
;      ---------------------------------------------------
;	- iu                 O           Unit number       
;       - SceneAMV          I/O          Structure containing SceneAMV_type

;
; Subroutines needed:
;       - None
;
;
; History:
;
;===============================================================
PRO ReadSceneAMV,iu, SceneAMV, print=printx
    ;----------------------------------------------------------------------------------
    ; Subroutine to write the Sceneamv file (either truth file or satellite-retrieved file).
    ;----------------------------------------------------------------------------

    ix3 = LONARR(3)
    rx7 = FLTARR(7)
    iqc = INTARR(SceneAMV.nqc)
    rqc = FLTARR(SceneAMV.nqc)
    rx2 = FLTARR(2) & rx = 9999. & ix5 = LONARR(5) & ix = 1L
    pflag =0
    cx  = '1234567890123456789012345678901234567890'
    IF (KEYWORD_SET(printx)) THEN pflag=1
    FOR iobs = 0L, SceneAMV.nProfiles-1L DO BEGIN
      IF (pflag AND iobs MOD 3000 eq 0) THEN print, iobs, FORMAT="('...Now reading iobs: ',i10)"   
      READU, iu, cx, ix3
      READU, iu, rx7, rqc, iqc, rx2, ix, rx, ix5
;       READU, iu, rx7
;       READU, iu, iqc
;       READU, iu, rx2, ix, rx, ix5
      sceneAMV.AlgDesc[iobs]  = cx
      sceneAMV.ProfIndx[iobs] = ix3[0]
      sceneAMV.WindCalcMethod[iobs] = ix3[1]
      sceneAMV.HeightMethod[iobs] = ix3[2]
      sceneAMV.angle[iobs]     = rx7[0]
      sceneAMV.WindSp[iobs]    = rx7[1]
      sceneAMV.WindDir[iobs]   = rx7[2]
      sceneAMV.WindPress[iobs] = rx7[3]
      sceneAMV.WindU[iobs]     = rx7[4]
      sceneAMV.WindV[iobs]     = rx7[5]
      sceneAMV.WindError[iobs] = rx7[6]
      sceneAMV.qc[iobs,0:sceneAMV.nqc-1] = iqc
      sceneAMV.qi[iobs,0:sceneAMV.nqc-1] = rqc
      sceneAMV.lat[iobs]       = rx2[0]
      sceneAMV.lon[iobs]       = rx2[1]
      sceneAMV.node[iobs]      = ix
      sceneAMV.scanUTC[iobs]   = rx
      sceneAMV.scanyear[iobs]  = ix5[0]
      sceneAMV.scanday[iobs]   = ix5[1]
      sceneAMV.scanmonth[iobs] = ix5[2]
      sceneAMV.iscanPos[iobs]  = ix5[3]
      sceneAMV.iscanLine[iobs] = ix5[4]
    ENDFOR
    FREE_LUN, iu
    CLOSE, iu
END

;===============================================================
; Name:		WritSceneAMV
;
;
; Type:		IDL Subroutine
;
;
; Description:  Writes the sceneamv with all
;               info coming from inputs
;
;
; Arguments:
;
;	    Name	    Type	    Description
;      ---------------------------------------------------
;	- iu                 O           Unit number       
;       - SceneAMV          I/O           Structure containing SceneAMV_type

;
; Subroutines needed:
;       - None
;
;
; History:
;
;===============================================================
PRO WritSceneAMV, iu, SceneAMV 
    ;----------------------------------------------------------------------------------
    ; Subroutine to write the Sceneamv file (either truth file or satellite-retrieved file).
    ;----------------------------------------------------------------------------

    ix3 = LONARR(3)
    rx7 = FLTARR(7)
    iqc = INTARR(SceneAMV.nqc)
    rqc = FLTARR(SceneAMV.nqc)
    rx2 = FLTARR(2) & rx = 9999. & ix5 = LONARR(5) & ix = 1L
    cx  = '1234567890123456789012345678901234567890'
    FOR iobs = 0L, SceneAMV.nProfiles-1L DO BEGIN
       
      ; build arrays for output 
      ; -----------------------
      ix3 = [sceneAMV.ProfIndx[iobs], sceneAMV.WindCalcMethod[iobs], sceneAMV.HeightMethod[iobs]]
      rx7 = [sceneAMV.angle[iobs], sceneAMV.WindSp[iobs], sceneAMV.WindDir[iobs], $
             sceneAMV.WindPress[iobs], sceneAMV.WindU[iobs], sceneAMV.WindV[iobs], $
             sceneAMV.WindError[iobs]]
      rqc = sceneAMV.qi[iobs,0:sceneAMV.nqc-1]
      iqc = sceneAMV.qc[iobs,0:sceneAMV.nqc-1]
      rx2 = [sceneAMV.lat[iobs], sceneAMV.lon[iobs]]
      ix  = sceneAMV.node[iobs]
      rx  = sceneAMV.scanUTC[iobs]
      ix5 = [sceneAMV.scanyear[iobs], sceneAMV.scanday[iobs], sceneAMV.scanmonth[iobs], $
             sceneAMV.iscanPos[iobs], sceneAMV.iscanLine[iobs]]
      cx  = sceneAMV.AlgDesc[iobs]
      ; write data to output logical unit
      ; ---------------------------------
      WRITEU, iu, cx, ix3
      WRITEU, iu, rx7, rqc, iqc, rx2, ix, rx, ix5
;       WRITEU, iu, cx, ix3
;       WRITEU, iu, rx7
;       WRITEU, iu, iqc
;       WRITEU, iu, rx2, ix, rx, ix5
      
    ENDFOR
    FREE_LUN, iu
    CLOSE, iu
END


