;---------------------------------------------------------------------------------
; Name:  generateConditionalData.pro
;
; Type:  IDL Program
;
; Description:
;   A local procedure used by main-level assessment tool code
;   to generate data in various conditions such as "clear sky", "cloudy sky", etc. 
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 31, 2014, DXu, Initial coding
;          Apr 24, 2014  ESMaddy, vectorized code, removed loops,
;                        no large-sparse arrays
;          Oct 20, 2014, MoharC, Added moisture Components
;
;---------------------------------------------------------------------------------
PRO generateConditionalData, filter, refRadObs, refRadSim, refSceneData, $
   refRadObsFilted, refRadSimFilted, refSceneDataFilted

   nfilter = N_ELEMENTS(filter)
   print, "size of filter ", nfilter
   
   index = LINDGEN(nfilter)
   RadObstags = TAG_NAMES(refRadObs)
   refRadObsFilted = CREATE_STRUCT(RadObstags, $
                                   refRadObs.scanPos[filter], $
                                   refRadObs.scanLine[filter], $
                                   refRadObs.lat[filter], $
                                   refRadObs.lon[filter], $
                                   refRadObs.nodeFlag[filter], $
                                   refRadObs.angle[filter], $
                                   refRadObs.solangle[filter], $
                                   refRadObs.QC[filter,*], $
                                   refRadObs.tb(filter, *), $
                                   refRadObs.tbDiff(filter, *),$
                                   refRadObs.nChan, $
                                   refRadObs.nQC)

   RadSimtags = TAG_NAMES(refRadSim)
   refRadSimFilted = CREATE_STRUCT(RadSimtags, $
                                   refRadSim.scanPos[filter], $
                                   refRadSim.scanLine[filter], $
                                   refRadSim.lat[filter], $
                                   refRadSim.lon[filter], $
                                   refRadSim.nodeFlag[filter], $
                                   refRadSim.angle[filter], $
                                   refRadSim.solangle[filter], $
                                   refRadSim.QC[filter,*], $
                                   refRadSim.tb(filter, *), $
                                   refRadSim.tbDiff(filter, *), $
                                   refRadObs.nChan, $
                                   refRadObs.nQC)

   Scenetags = TAG_NAMES(refSceneData)
   refSceneDataFilted = CREATE_STRUCT(SceneTags, $
                                   refSceneData.tpwVec[filter], $
                                   refSceneData.clwVec[filter],$
                                   refSceneData.rwpVec[filter],$
                                   refSceneData.gwpVec[filter],$
                                   refSceneData.tSkinVec[filter],$
                                   refSceneData.sfcTypeVec[filter],$
                                   refSceneData.windSpeedVec[filter], $
                                   refSceneData.emissvec[filter,*], $  ; should be emis
                                   refSceneData.emissanlvec[filter,*], $  ; should be analytic emissivity
                                   refSceneData.wv950Vec[filter],$ 
                                   refSceneData.wv500Vec[filter],$  
                                   refSceneData.wv300Vec[filter])
   return 

   ; - no large sparse arrays  
   refRadObsFilted.scanPos[index]   = refRadObs.scanPos[filter]
   refRadObsFilted.scanLine(index)  = refRadObs.scanLine[filter]
   refRadObsFilted.lat(index)       = refRadObs.lat[filter]
   refRadObsFilted.lon(index)       = refRadObs.lon[filter]
   refRadObsFilted.nodeFlag(index)  = refRadObs.nodeFlag[filter]
   refRadObsFilted.angle(index)     = refRadObs.angle[filter]
   refRadObsFilted.QC(index)        = refRadObs.QC[filter]
   refRadObsFilted.tb(index, *)     = refRadObs.tb(filter, *)
   refRadObsFilted.tbDiff(index, *) = refRadObs.tbDiff(filter, *)
   refRadSimFilted.scanPos(index)   = refRadSim.scanPos[filter]
   refRadSimFilted.scanLine(index)  = refRadSim.scanLine[filter]
   refRadSimFilted.lat(index)       = refRadSim.lat[filter]
   refRadSimFilted.lon(index)       = refRadSim.lon[filter]
   refRadSimFilted.nodeFlag(index)  = refRadSim.nodeFlag[filter]
   refRadSimFilted.angle(index)     = refRadSim.angle[filter]
   refRadSimFilted.QC(index)        = refRadSim.QC[filter]
   refRadSimFilted.tb(index, *)     = refRadSim.tb(filter, *)
   refRadSimFilted.tbDiff(index, *) = refRadSim.tbDiff(filter, *)
   
   refSceneDataFilted.tpwVec(index)     = refSceneData.tpwVec[filter]
   refSceneDataFilted.clwVec(index)     = refSceneData.clwVec[filter]
   refSceneDataFilted.rwpVec(index)     = refSceneData.rwpVec[filter]
   refSceneDataFilted.gwpVec(index)     = refSceneData.gwpVec[filter]
   refSceneDataFilted.tSkinVec(index)   = refSceneData.tSkinVec[filter]
   refSceneDataFilted.sfcTypeVec(index) = refSceneData.sfcTypeVec[filter]
   refSceneDataFilted.wv950Vec(index) = refSceneData.wv950Vec[filter]
   refSceneDataFilted.wv500Vec(index) = refSceneData.wv500Vec[filter]
   refSceneDataFilted.wv300Vec(index) = refSceneData.wv300Vec[filter]   
   return 
   ; - no loops
   FOR i = 0, N_ELEMENTS(filter) - 1 DO BEGIN
      ; Get original index 
      j= filter(i)
      refRadObsFilted.scanPos(i)   = refRadObs.scanPos(j)
      refRadObsFilted.scanLine(i)  = refRadObs.scanLine(j)
      refRadObsFilted.lat(i)       = refRadObs.lat(j)
      refRadObsFilted.lon(i)       = refRadObs.lon(j)
      refRadObsFilted.nodeFlag(i)  = refRadObs.nodeFlag(j)
      refRadObsFilted.angle(i)     = refRadObs.angle(j)
      refRadObsFilted.QC(i)        = refRadObs.QC(j)
      refRadObsFilted.tb(i, *)     = refRadObs.tb(j, *)
      refRadObsFilted.tbDiff(i, *) = refRadObs.tbDiff(j, *)

      refRadSimFilted.scanPos(i)   = refRadSim.scanPos(j)
      refRadSimFilted.scanLine(i)  = refRadSim.scanLine(j)
      refRadSimFilted.lat(i)       = refRadSim.lat(j)
      refRadSimFilted.lon(i)       = refRadSim.lon(j)
      refRadSimFilted.nodeFlag(i)  = refRadSim.nodeFlag(j)
      refRadSimFilted.angle(i)     = refRadSim.angle(j)
      refRadSimFilted.QC(i)        = refRadSim.QC(j)
      refRadSimFilted.tb(i, *)     = refRadSim.tb(j, *)
      refRadSimFilted.tbDiff(i, *) = refRadSim.tbDiff(j, *)

      refSceneDataFilted.tpwVec(i)     = refSceneData.tpwVec(j)
      refSceneDataFilted.clwVec(i)     = refSceneData.clwVec(j)
      refSceneDataFilted.rwpVec(i)     = refSceneData.rwpVec(j)
      refSceneDataFilted.gwpVec(i)     = refSceneData.gwpVec(j)
      refSceneDataFilted.tSkinVec(i)   = refSceneData.tSkinVec(j)
      refSceneDataFilted.sfcTypeVec(i) = refSceneData.sfcTypeVec(j)
      refSceneDataFilted.wv950Vec(i) = refSceneData.wv950Vec(j)
      refSceneDataFilted.wv500Vec(i) = refSceneData.wv500Vec(j)
      refSceneDataFilted.wv300Vec(i) = refSceneData.wv300Vec(j)

   ENDFOR

END
