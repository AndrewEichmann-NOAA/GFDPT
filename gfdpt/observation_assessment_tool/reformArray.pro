;---------------------------------------------------------------------------------
; Name:  reformArray.pro
;
; Type:  IDL Program
;
; Description:
;   A locally-defined procedure used by the assessment tool main-level code
;   to combine the data of all the orbits into one big array. 
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 5, 2014, DXu, Initial coding
;          Oct 29, 2014, ESM, Added solar angles 
;
;---------------------------------------------------------------------------------
PRO reformArray, MAX_FOV, nOrbits,  $
   sensorID, radObs, radSim, refRadObs, refRadSim, $
   sceneData, refSceneData

   ; Define struct to hold reformed data.
   ;-------------------------------------------------------
   ; Convert 2-D (Fov X File) into 1-D array ( Fov * File )
   ; Result:
   ;     file 1        file 2             file n
   ;   [ Fov * File ][ Fov * File ] ... [ Fov * File ]
   ;-------------------------------------------------------
   refRadObs.scanPos  = reform(radObs.scanPos(*, *), nOrbits * MAX_FOV)
   refRadObs.scanLine = reform(radObs.scanLine(*, *), nOrbits * MAX_FOV)
   refRadObs.lat      = reform(radObs.lat(*, *), nOrbits * MAX_FOV)
   refRadObs.lon      = reform(radObs.lon(*, *), nOrbits * MAX_FOV)
   refRadObs.nodeFlag = reform(radObs.dir(*, *), nOrbits * MAX_FOV)
   refRadObs.angle    = reform(radObs.angle(*, *), nOrbits * MAX_FOV)
   refRadObs.solangle    = reform(radObs.solangle(*, *), nOrbits * MAX_FOV)

   refRadSim.scanPos  = reform(radSim.scanPos(*, *), nOrbits * MAX_FOV)
   refRadSim.scanLine = reform(radSim.scanLine(*, *), nOrbits * MAX_FOV)
   refRadSim.lat      = reform(radSim.lat(*, *), nOrbits * MAX_FOV)
   refRadSim.lon      = reform(radSim.lon(*, *), nOrbits * MAX_FOV)
   refRadSim.nodeFlag = reform(radSim.dir(*, *), nOrbits * MAX_FOV)
   refRadSim.angle    = reform(radSim.angle(*, *), nOrbits * MAX_FOV)
   refRadSim.solangle    = reform(radObs.solangle(*, *), nOrbits * MAX_FOV)

   refSceneData.tpwVec = reform(sceneData.tpwVec(*, *), nOrbits * MAX_FOV)
   refSceneData.clwVec = reform(sceneData.clwVec(*, *), nOrbits * MAX_FOV)
   refSceneData.rwpVec = reform(sceneData.rwpVec(*, *), nOrbits * MAX_FOV)
   refSceneData.gwpVec = reform(sceneData.gwpVec(*, *), nOrbits * MAX_FOV)
   refSceneData.tSkinVec = reform(sceneData.tSkinVec(*, *), nOrbits * MAX_FOV)
   refSceneData.sfcTypeVec = reform(sceneData.sfcTypeVec(*, *), nOrbits * MAX_FOV)
   refsceneData.wv950Vec = reform(sceneData.wv950Vec, nOrbits * MAX_FOV)
   refsceneData.wv500Vec = reform(sceneData.wv500Vec, nOrbits * MAX_FOV)
   refsceneData.wv300Vec = reform(sceneData.wv300Vec, nOrbits * MAX_FOV)
;   refsceneData.wv500Vec = scene.AbsorbLayVec(pidx,75,0)
;   refsceneData.wv300Vec = scene.AbsorbLayVec(pidx,63,0)

   ;--------------------------
   ; 3-d arrays conversion
   ;--------------------------
   ; Convert 3-D (Fov X File X Channel )
   ; into 2-D array ( (Fov * File) X Channel )
   ;   file 1        file 2             file n
   ; [ Fov * File ][ Fov * File ] ... [ Fov * File ]  <= chan 1
   ; [ Fov * File ][ Fov * File ] ... [ Fov * File ]  <= chan 2
   ;   ...
   ; [ Fov * File ][ Fov * File ] ... [ Fov * File ]  <= chan n
   ;
   refRadObs.nchan = radObs.nchan
   refRadObs.nQC = radObs.nQC
   QC       = reform(radObs.QC(*, *,0), nOrbits * MAX_FOV)
   IF (sensorID eq 'gmi') THEN BEGIN
     i1 = SIZE(radObs.QC,/DIMENSION) 
     nQCl = i1[2]
     IF (nQCl gt 1) THEN BEGIN
       ;--- RFI 10GHz
       QC2   = reform(radObs.QC(*, *,1), nOrbits * MAX_FOV)
       ;--- RFI 18GHz
       QC3   = reform(radObs.QC(*, *,2), nOrbits * MAX_FOV)
     ENDIF
   ENDIF
   ; QC       = reform(radSim.QC(*, *), nOrbits * MAX_FOV)
   ; check the first QC element for good/bad data
   idx = WHERE(QC ne 0, nidx)
   FOR iChan = 0L, radObs.nChan - 1 DO BEGIN
      refRadObs.tb(*, iChan) = reform(radObs.tb(*, *, iChan), nOrbits * MAX_FOV)
      IF (nidx ne 0) THEN refradObs.tb[idx,ichan] = -999.
      refRadSim.tb(*, iChan) = reform(radSim.tb(*, *, iChan), nOrbits * MAX_FOV)
      ; O - B, and save the same tbDiff into refRadObs and refRadSim
      refRadObs.tbDiff(*, iChan) = refRadObs.tb(*, iChan) - refRadSim.tb(*, iChan)
      refRadSim.tbDiff(*, iChan) = refRadObs.tbDiff(*, iChan)
   ENDFOR
   FOR iQC = 0L, radObs.nQC - 1 DO $
      refRadObs.qc[*, iQC] = reform(radObs.qc[*,*,iQC], nOrbits*MAX_FOV )

   ;--now do emissivity 
   FOR iChan = 0L, radObs.nChan - 1 DO BEGIN
     refsceneData.emissvec[*,iChan] = reform(sceneData.emissvec[*,*,iChan], nOrbits * MAX_FOV)
     refsceneData.emissanlvec[*,iChan] = reform(sceneData.emissanlvec[*,*,iChan], nOrbits * MAX_FOV)
   ENDFOR
END
