;---------------------------------------------------------------------------------
; Name:  defineDataStructure.pro
;
; Type:  IDL Program
;
; Description:
;   To define data structures used to hold various data.
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 26, 2014, DXu, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO defineRadDataType, MAX_FOV, nOrbits, MAX_CHAN, MAX_QC, radData

   radData={ RadDataType, $
      nFOV : lonarr(nOrbits),$ ; number of profiles per file
      scanPos  : intarr(MAX_FOV,nOrbits), $  ; pos per file
      scanLine : intarr(MAX_FOV,nOrbits), $  ; line per file
      lat   : fltarr(MAX_FOV,nOrbits),$  ; lat per file
      lon   : fltarr(MAX_FOV,nOrbits),$  ; lon per file
      dir   : fltarr(MAX_FOV,nOrbits),$  ; dir per file
      angle : fltarr(MAX_FOV,nOrbits),$  ; ang per file
      solangle : fltarr(MAX_FOV,nOrbits),$  ; solar angle per file
      QC    : lonarr(MAX_FOV,nOrbits,MAX_QC),$  ; QC  per file
      tb    : fltarr(MAX_FOV,nOrbits,MAX_CHAN),$ ; tb per file per channel
      nChan : 0L, nQC : 0L}

END

PRO defineSceneDataType, MAX_FOV, MAX_CHAN, nOrbits, sceneData

   sceneData = { SceneDataType, $
      tpwVec    : fltarr(MAX_FOV, nOrbits),  $
      clwVec    : fltarr(MAX_FOV, nOrbits),  $
      rwpVec    : fltarr(MAX_FOV, nOrbits),  $
      gwpVec    : fltarr(MAX_FOV, nOrbits),  $
      tSkinVec  : fltarr(MAX_FOV, nOrbits),  $
      sfcTypeVec : lonarr(MAX_FOV, nOrbits), $
      windSpeedVec : fltarr(MAX_FOV, nOrbits), $
      emissVec : fltarr(MAX_FOV, nOrbits, MAX_CHAN), $
      emissanlVec : fltarr(MAX_FOV, nOrbits, MAX_CHAN), $
      wv950Vec   : fltarr(MAX_FOV, nOrbits), $
      wv500Vec   : fltarr(MAX_FOV, nOrbits), $
      wv300Vec   : fltarr(MAX_FOV, nOrbits)}
                 

END

PRO defineRefSceneDataType, MAX_FOV, MAX_CHAN, nOrbits, refsceneData

   refsceneData = { RefSceneDataType, $
      tpwVec    : fltarr(MAX_FOV* nOrbits),  $
      clwVec    : fltarr(MAX_FOV* nOrbits),  $
      rwpVec    : fltarr(MAX_FOV* nOrbits),  $
      gwpVec    : fltarr(MAX_FOV* nOrbits),  $
      tSkinVec  : fltarr(MAX_FOV* nOrbits),  $
      sfcTypeVec : lonarr(MAX_FOV* nOrbits), $
      windSpeedVec : fltarr(MAX_FOV* nOrbits), $
      emissVec : fltarr(MAX_FOV*nOrbits, MAX_CHAN), $
      emissanlVec : fltarr(MAX_FOV*nOrbits, MAX_CHAN), $
      wv950Vec   : fltarr(MAX_FOV* nOrbits), $
      wv500Vec   : fltarr(MAX_FOV* nOrbits), $
      wv300Vec   : fltarr(MAX_FOV* nOrbits)}

END

PRO defineRefRadDataType, MAX_FOV, nOrbits, MAX_CHAN, MAX_QC, refRadData

   refRadData = { RefRadDataType,  $
      scanPos  : intarr(MAX_FOV * nOrbits),$
      scanLine : intarr(MAX_FOV * nOrbits),$
      lat      : fltarr(MAX_FOV * nOrbits),$
      lon      : fltarr(MAX_FOV * nOrbits),$
      nodeFlag : fltarr(MAX_FOV * nOrbits),$
      angle    : fltarr(MAX_FOV * nOrbits),$
      solangle : fltarr(MAX_FOV * nOrbits),$  ; solar angle per file
      QC       : lonarr(MAX_FOV * nOrbits,MAX_QC),$
      tb       : fltarr(MAX_FOV * nOrbits,MAX_CHAN), $
      tbDiff   : fltarr(MAX_FOV * nOrbits,MAX_CHAN), $
      nChan    : 0L,   nQC:  0L}

END
