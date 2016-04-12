;---------------------------------------------------------------------------------
; Name:  initializeData.pro
;
; Type:  IDL Program
;
; Description:
;   To initialize data structure with FILL values
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 26, 2014, DXu, Initial coding
;
;
;---------------------------------------------------------------------------------
@defineDataStructure.pro

PRO initializeRadDataType, MAX_FOV, nOrbits, MAX_CHAN, MAX_QC, radData
   INT_FILL_VALUE = -999
   LONG_FILL_VALUE = -999L
   FLOAT_FILL_VALUE = -999.99

   radData.nFOV = MAKE_ARRAY(nOrbits, /LONG, VALUE = LONG_FILL_VALUE)
   radData.scanPos  = MAKE_ARRAY(MAX_FOV, nOrbits, /INTEGER, VALUE = INT_FILL_VALUE)
   radData.scanLine = MAKE_ARRAY(MAX_FOV, nOrbits, /INTEGER, VALUE = INT_FILL_VALUE)
   radData.lat   = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   radData.lon   = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   radData.dir   = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   radData.angle = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   radData.solangle = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   radData.QC    = MAKE_ARRAY(MAX_FOV, nOrbits, MAX_QC, /LONG, VALUE = LONG_FILL_VALUE)
   radData.tb    = MAKE_ARRAY(MAX_FOV, nOrbits, MAX_CHAN, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   radData.nChan = LONG_FILL_VALUE
END

PRO initializeRefRadDataType, MAX_FOV, nOrbits, MAX_CHAN, MAX_QC, refRadData
   INT_FILL_VALUE = -999
   LONG_FILL_VALUE = -999L
   FLOAT_FILL_VALUE = -999.99

   refRadData.scanPos  = MAKE_ARRAY(MAX_FOV * nOrbits, /INTEGER, VALUE = INT_FILL_VALUE)
   refRadData.scanLine = MAKE_ARRAY(MAX_FOV * nOrbits, /INTEGER, VALUE = INT_FILL_VALUE)
   refRadData.lat      = MAKE_ARRAY(MAX_FOV * nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   refRadData.lon      = MAKE_ARRAY(MAX_FOV * nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   refRadData.nodeFlag = MAKE_ARRAY(MAX_FOV * nOrbits, /LONG, VALUE = LONG_FILL_VALUE)
   refRadData.angle    = MAKE_ARRAY(MAX_FOV * nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   refradData.solangle = MAKE_ARRAY(MAX_FOV * nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   refRadData.QC       = MAKE_ARRAY(MAX_FOV * nOrbits, MAX_QC, $
                                    /LONG, VALUE = LONG_FILL_VALUE)
   refRadData.tb       = MAKE_ARRAY(MAX_FOV * nOrbits, MAX_CHAN, $
                             /FLOAT, VALUE = FLOAT_FILL_VALUE)
   refRadData.tbDiff    = MAKE_ARRAY(MAX_FOV * nOrbits, MAX_CHAN, $
                             /FLOAT, VALUE = FLOAT_FILL_VALUE)

END

PRO initializeRefSceneDataType, MAX_FOV, MAX_CHAN, nOrbits,  sceneData
   LONG_FILL_VALUE = -999L
   FLOAT_FILL_VALUE = -999.99

   sceneData.tpwVec    = MAKE_ARRAY(MAX_FOV*nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.clwVec    = MAKE_ARRAY(MAX_FOV*nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE) 
   sceneData.rwpVec    = MAKE_ARRAY(MAX_FOV*nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.gwpVec    = MAKE_ARRAY(MAX_FOV*nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.tSkinVec  = MAKE_ARRAY(MAX_FOV*nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.sfcTypeVec = MAKE_ARRAY(MAX_FOV*nOrbits, /LONG, VALUE = LONG_FILL_VALUE)
   sceneData.windSpeedVec = MAKE_ARRAY(MAX_FOV*nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.emissvec     = MAKE_ARRAY(MAX_FOV*nOrbits, MAX_CHAN, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.emissanlvec  = MAKE_ARRAY(MAX_FOV*nOrbits, MAX_CHAN, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.wv950Vec  = MAKE_ARRAY(MAX_FOV*nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.wv500Vec  = MAKE_ARRAY(MAX_FOV*nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.wv300Vec  = MAKE_ARRAY(MAX_FOV*nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
END

PRO initializeSceneDataType, MAX_FOV, MAX_CHAN, nOrbits,  sceneData
   LONG_FILL_VALUE = -999L
   FLOAT_FILL_VALUE = -999.99

   sceneData.tpwVec    = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.clwVec    = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE) 
   sceneData.rwpVec    = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.gwpVec    = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.tSkinVec  = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.sfcTypeVec = MAKE_ARRAY(MAX_FOV, nOrbits, /LONG, VALUE = LONG_FILL_VALUE)
   sceneData.windSpeedVec = MAKE_ARRAY(MAX_FOV, nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.emissvec  = MAKE_ARRAY(MAX_FOV, nOrbits, MAX_CHAN, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.emissanlvec = MAKE_ARRAY(MAX_FOV, nOrbits, MAX_CHAN, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.wv950Vec  = MAKE_ARRAY(MAX_FOV,nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.wv500Vec  = MAKE_ARRAY(MAX_FOV,nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
   sceneData.wv300Vec  = MAKE_ARRAY(MAX_FOV,nOrbits, /FLOAT, VALUE = FLOAT_FILL_VALUE)
END

PRO initializeAll, MAX_FOV, nOrbits, MAX_CHAN, MAX_QC, $ 
    radObs, radSim, sceneData, $
    refRadObs, refRadSim, refSceneData 

   ; Define types and initialize to default value of each type.
   defineRadDataType, MAX_FOV, nOrbits, MAX_CHAN, MAX_QC, radObs
   defineSceneDataType, MAX_FOV, MAX_CHAN, nOrbits,  sceneData
   defineRefRadDataType, MAX_FOV, nOrbits, MAX_CHAN, MAX_QC, refRadObs
   defineRefSceneDataType, MAX_FOV, MAX_CHAN, nOrbits, refsceneData

   ; Create objects of types
   radSim = CREATE_STRUCT(NAME = 'RadDataType')
   refRadSim = CREATE_STRUCT(NAME = 'RefRadDataType')
   refSceneData = CREATE_STRUCT(NAME = 'RefSceneDataType')

   ; Initialize data to FILL values
   ; Data initialization
   initializeRadDataType, MAX_FOV, nOrbits, MAX_CHAN, MAX_QC, radObs
   initializeRadDataType, MAX_FOV, nOrbits, MAX_CHAN, MAX_QC, radSim
   initializeSceneDataType, MAX_FOV, MAX_CHAN, nOrbits,  sceneData

   ; Reformed data initialization
   initializeRefRadDataType, MAX_FOV, nOrbits, MAX_CHAN, MAX_QC, refRadObs
   initializeRefRadDataType, MAX_FOV, nOrbits, MAX_CHAN, MAX_QC, refRadSim
   initializeRefSceneDataType, MAX_FOV, MAX_CHAN, nOrbits,  refSceneData

END
