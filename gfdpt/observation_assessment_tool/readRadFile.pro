;---------------------------------------------------------------------------------
; Name:  readRadFile.pro
;
; Type:  IDL Program
;
; Description:
;   To read radiance files using IDL code in MIRS trunk.
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 5, 2014, DXu, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO readRadFile, nOrbits, MAX_FOV, MAX_CHAN, MAX_QC,   $
    radFileList1, radFileList2, sceneFileList, $ 
    radObs, radSim, sceneData, oldscene=oldscenex

   ;---------------------------------------
   ; step 1:
   ;   Read FMSRC file (observed radiance)
   ;---------------------------------------
   ;
   ; Loop thru. files/orbits
   nall = 0L
   FOR iFile = 0L, nOrbits - 1 DO BEGIN
      PRINT,'------------------------------------------------'
      PRINT,'Orbit ',iFile, " is being processed : "
      PRINT,'   observed radiance orbit file :',radFileList1(iFile)

      ;
      ; Read radiance from a pair of files to rad1 and rad2
      ;
      ; LoadRadFile I, I , O , X
      PRINT, "Start to open file to read ............" 
      LoadRadFile,1,radFileList1(iFile),rad1,0
      PRINT, "End of reading  file to read *****************"

      PRINT, "Number of files                   : ", rad1.nFilesRad
      PRINT, "Number of profiles in file        : ", rad1.nProf
      PRINT, "Number of channels in file        : ", rad1.nChan
      PRINT, "Number of scan positions per line : ", rad1.nPosScan
      PRINT, "Number of scan lines              : ", rad1.nScanLines
      PRINT,'------------------------------------------------'
      ; Save number of channels 
      IF (iFile eq 0L ) THEN BEGIN
         radObs.nChan = rad1.nChan
         radObs.nQC = rad1.nQC
      ENDIF

      ; Save total number of FOVs in a file
      radObs.nFOV(iFile) = rad1.nProf

      pidx = LINDGEN(rad1.nProf)
      qcidx = nall + LINDGEN(rad1.nProf)
      nall += rad1.nProf
      ; Loop thru. FOVs within orbit
      radObs.scanPos(pidx, iFile) = rad1.scanPos(0, pidx)
      radObs.scanLine(pidx, iFile) = rad1.scanLine(0, pidx)
      radObs.lat(pidx, iFile)  = rad1.lat(0, pidx)
      radObs.lon(pidx, iFile)  = rad1.lon(0, pidx)
      radObs.dir(pidx, iFile)  = rad1.direc(0, pidx)
      ; Get the 1st QC in the 1st orbit
;      radObs.QC(pidx, iFile)   = rad1.QC(0,pidx,0)
      ; all QC In the file
      nQC = rad1.nQC
      IF (nQC gt MAX_QC) THEN nQC = MAX_QC
      radObs.QC(pidx,iFile,0:nQC-1) = rad1.QC(0,pidx,0:nQC-1)
      ; Get tb for each channel
      radObs.tb(pidx, iFile, 0 : rad1.nChan - 1)   $
         = rad1.tb(0, pidx, 0 : rad1.nChan - 1)
      ; Average angles over all channels to compute mean angle
      radObs.angle(pidx, iFile)  = total(rad1.angle(0, pidx, 0 : rad1.nChan - 1),3)/FLOAT(rad1.nchan)
      radObs.solangle(pidx, iFile)  = rad1.solzenangle(0, pidx)
      ; Loop thru. FOVs within orbit - no loops
      ;; FOR iProf = 0L, rad1.nProf - 1 DO BEGIN
      ;; ENDFOR
   ENDFOR

   ;---------------------------------------
   ; step 2:
   ;   Read FWD file   (simulated radiance)
   ;---------------------------------------
   ;
   ; Loop thru. files/orbits
   FOR iFile = 0L, nOrbits - 1 DO BEGIN
      PRINT,'------------------------------------------------'
      PRINT,'Orbit ',iFile, " is being processed : "
      PRINT,'   simulated radiance orbit file :',radFileList2(iFile)

      ;
      ; Read radiance from rad2
      ;
      ; LoadRadFile I, I , O , X
      LoadRadFile,1,radFileList2(iFile),rad2,0

      PRINT, "Number of files                   : ", rad2.nFilesRad
      PRINT, "Number of profiles in file        : ", rad2.nProf
      PRINT, "Number of channels in file        : ", rad2.nChan
      PRINT, "Number of scan positions per line : ", rad2.nPosScan
      PRINT, "Number of scan lines              : ", rad2.nScanLines
      PRINT,'------------------------------------------------'
      ; Save number of channels 
      IF (iFile eq 0L ) THEN BEGIN
         radSim.nChan = rad2.nChan
      ENDIF

      ; Save total number of FOVs in a file
      radSim.nFOV(iFile) = rad2.nProf

      pidx = LINDGEN(rad2.nProf)
      radSim.scanPos(pidx, iFile) = rad2.scanPos(0, pidx)
      radSim.scanLine(pidx, iFile) = rad2.scanLine(0, pidx)
      radSim.lat(pidx, iFile)  = rad2.lat(0, pidx)
      radSim.lon(pidx, iFile)  = rad2.lon(0, pidx)
      radSim.dir(pidx, iFile)  = rad2.direc(0, pidx)
      ; Get the 1st QC in the 1st orbit
      radSim.QC(pidx, iFile, 0)  = rad2.QC(0,pidx,0)
      ; Get tb for each channel
      radSim.tb(pidx, iFile, 0 : rad2.nChan - 1)   $
	     = rad2.tb(0, pidx, 0 : rad2.nChan - 1)
      ; Average angles over all channels to compute mean angle
      radSim.angle(pidx, iFile)  = total(rad2.angle(0, pidx, 0 : rad2.nChan - 1),3)/FLOAT(rad2.nchan)
      radObs.solangle(pidx, iFile)  = rad2.solzenangle(0, pidx)

      ; Loop thru. FOVs within orbit - no loops
      ;; FOR iProf = 0L, rad2.nProf - 1 DO BEGIN
      ;;    radSim.angle(iProf, iFile)  = mean(rad2.angle(0, iProf, 0 : rad2.nChan - 1))
      ;; ENDFOR
   ENDFOR

   ;---------------------------------------
   ; step 3:
   ;   Read scene data (GFS 6-hr forecast)
   ;---------------------------------------
   ;
   ; Loop thru. files/orbits
   FOR iFile = 0L, nOrbits - 1 DO BEGIN
      ;
      ; Read scene data
      ;
      PRINT,'------------------------------------------------'
      PRINT,'Orbit ',iFile, " is being processed : "
      IF (KEYWORD_SET(oldscenex)) THEN BEGIN
         PRINT,'   scene file :', sceneFileList(iFile)
         LoadSceneFile, sceneFileList(iFile), topID, scene, 100000000L 
      ENDIF ELSE BEGIN
         PRINT,'   new scene file :', sceneFileList(iFile)
         LoadSceneDump, sceneFileList(iFile), scene
      ENDELSE
      PRINT, "Number of profiles in file        : ", scene.nprof
      PRINT,'------------------------------------------------'
      ; Save the total number of profiles 
      total_num = scene.nprof
      pidx = LINDGEN(total_num)
      ; scene data structure changes based
      ; on whether next file number of
      ; profiles is larger than previous  
      ; make sure the indexing is correct for the current scene
      ; structure esm -- 140501
      sceneData.tpwVec(pidx, iFile) = scene.tpwVec(pidx) 
      sceneData.clwVec(pidx, iFile) = scene.clwVec(pidx)
      sceneData.rwpVec(pidx, iFile) = scene.rwpVec(pidx) 
      sceneData.gwpVec(pidx, iFile) = scene.gwpVec(pidx) 
      sceneData.tSkinVec(pidx, iFile) = scene.tSkinVec(pidx) 
      sceneData.sfcTypeVec(pidx, iFile) = scene.sfcTypVec(pidx) 
      IF (TAG_EXIST(scene,'AbsorbLayVec')) THEN BEGIN
        sceneData.wv950Vec(pidx, iFile) = scene.AbsorbLayVec(pidx,94,0)
        sceneData.wv500Vec(pidx, iFile) = scene.AbsorbLayVec(pidx,75,0)
        sceneData.wv300Vec(pidx, iFile) = scene.AbsorbLayVec(pidx,63,0)
      ENDIF
      ;---emissivity and analytic emissivity from the file
      IF (TAG_EXIST(scene,'EmissVec')) THEN BEGIN
        sceneData.EmissVec(pidx, iFile, *)    = scene.EmissVec(pidx,*)
        sceneData.EmissAnlVec(pidx, iFile, *) = scene.EmissAnlVec(pidx,*)
      ENDIF
         
;      sceneData.windSpeedVec(pidx, iFile) = SQRT(scene.windUvec(pidx)^2 + scene.windVvec(pidx)^2)

      ; Loop thru. FOVs within orbit - no loops
;      FOR iProf=0L, total_num - 1 DO BEGIN
;      ENDFOR
   ENDFOR

   ;; !ORDER = 0
   ;; pstr = '19H GHz'
   ;; img_name='f18_emissivity' + pstr + '_2014-10-05.png'
   ;; title = 'F18 emissivity @ ' + pstr +' 2014-10-05'
   ;; tgvmin = 0. & tgvmax = 1.0
   ;; ctit   = 'emis, [1]'
   ;; minLat = -90 & maxLat = 90
   ;; minLon = -180 & maxLon = 180
   ;; emis  = REFORM(sceneData.EmissVec[*,*,11])
   ;; emisa = REFORM(sceneData.EmissAnlVec[*,*,11])
   ;; y = RadObs.lat
   ;; x = Radobs.lon
   
   ;; ind = where(y ge minLat and y le maxLat and $
   ;;             emis gt 0.0)
 
   ;; mapPlot_png,minLat,maxLat,minLon,maxLon,y,x,ind,title, $
   ;;             tgvmin,tgvmax,emis,ctit,0.6,8,1,0,'(f7.1)',8,$
   ;;             img_name,color_table_index=33
   ;; map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
   
   ;; img_name='f18_emissivityanl' + pstr + '_2014-10-05.png'
   ;; title = 'F18 analytic emissivity @ ' + pstr +' 2014-10-05'
   ;; mapPlot_png,minLat,maxLat,minLon,maxLon,y,x,ind,title, $
   ;;             tgvmin,tgvmax,emisa,ctit,0.6,8,1,0,'(f7.1)',8,$
   ;;             img_name,color_table_index=33
   ;; map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
   ;; stop
END
