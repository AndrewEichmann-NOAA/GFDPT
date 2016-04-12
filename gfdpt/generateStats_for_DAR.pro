PRO generateStats_for_DAR, refRadObs, refRadSim, sfctype, $
                           chPlotArr, inputConfig, stats

    ; Get input config params
    chanNumArr = inputConfig.chanNumArr
    prefix  = inputConfig.prefix
    MIN_LAT = inputConfig.MIN_LAT
    MAX_LAT = inputConfig.MAX_LAT
    MIN_LON = inputConfig.MIN_LON
    MAX_LON = inputConfig.MAX_LON
    minBT_Values = inputConfig.minBT_Values
    maxBT_Values = inputConfig.maxBT_Values

    ; Get num of channels to calculate stats
    nchan = N_ELEMENTS(chPlotArr)
  
    FOR ichan=0,nchan-1 DO BEGIN
      ; check for good data
      filter_Alln = WHERE(refRadObs.lat GE MIN_LAT           $
                           AND refRadObs.lat LE MAX_LAT                $
                           AND refRadObs.tb(*,chPlotArr(iChan)) GT minBT_Values[chPlotArr(ichan)] $
                           AND refRadObs.tb(*,chPlotArr(iChan)) LT maxBT_Values[chPlotArr(ichan)] $
                           AND refRadSim.lat GE MIN_LAT                $
                           AND refRadSim.lat LE MAX_LAT                $
                           AND refRadSim.tb(*,chPlotArr(iChan)) GT minBT_Values[chPlotArr(ichan)] $
                           AND refRadSim.tb(*,chPlotArr(iChan)) LT maxBT_Values[chPlotArr(ichan)] $
                           AND sfctype eq 0, nAlln )
      ; no good data continue 
      IF (nAlln eq 0) THEN continue 
      x1 = REFORM(refRadObs.tbdiff[filter_Alln,chPlotArr[ichan]])
      ro = REFORM(refRadObs.tb[filter_Alln,chPlotArr[ichan]])
      rs = REFORM(refRadSim.tb[filter_Alln,chPlotArr[ichan]])
;      IF (ichan eq 23) THEN stop
      ; compute stats
      stats.bias_all[ichan] = MEAN(x1)
      stats.sdv_all[ichan]  = sQRT(VARIANCE(x1))
      stats.rms_all[ichan]  = SQRT(stats.bias_all[ichan]^2. + stats.sdv_all[ichan]^2.)
      stats.correl_all[ichan] = CORRELATE(ro,rs)
      stats.n_all[ichan] = nAlln
      
   ENDFOR
 
   FOR ichan=0,nchan-1 DO BEGIN
      ; check for good data
      filter_asc = WHERE(refRadObs.lat GE MIN_LAT           $
                           AND refRadObs.lat LE MAX_LAT                $
                           AND refRadObs.tb(*,chPlotArr(iChan)) GT minBT_Values[chPlotArr(ichan)] $
                           AND refRadObs.tb(*,chPlotArr(iChan)) LT maxBT_Values[chPlotArr(ichan)] $
                           AND refRadSim.lat GE MIN_LAT                $
                           AND refRadSim.lat LE MAX_LAT                $
                           AND refRadSim.tb(*,chPlotArr(iChan)) GT minBT_Values[chPlotArr(ichan)] $
                           AND refRadSim.tb(*,chPlotArr(iChan)) LT maxBT_Values[chPlotArr(ichan)] $
                           AND sfctype eq 0 $
                           AND refRadSim.nodeflag eq 0, nAlla )
      ; no good data continue 
      IF (nAlla eq 0) THEN continue 
      x1 = REFORM(refRadObs.tbdiff[filter_asc,chPlotArr[ichan]])
      ro = REFORM(refRadObs.tb[filter_asc,chPlotArr[ichan]])
      rs = REFORM(refRadSim.tb[filter_asc,chPlotArr[ichan]])

      ; compute stats
      stats.bias_asc[ichan] = MEAN(x1)
      stats.sdv_asc[ichan]  = sQRT(VARIANCE(x1))
      stats.rms_asc[ichan]  = SQRT(stats.bias_asc[ichan]^2. + stats.sdv_asc[ichan]^2.)
      stats.correl_asc[ichan] = CORRELATE(ro,rs)
      stats.n_asc[ichan] = nAlla
    ENDFOR

    FOR ichan=0,nchan-1 DO BEGIN
      ; check for good data
      filter_desc = WHERE(refRadObs.lat GE MIN_LAT           $
                           AND refRadObs.lat LE MAX_LAT                $
                           AND refRadObs.tb(*,chPlotArr(iChan)) GT minBT_Values[chPlotArr(ichan)] $
                           AND refRadObs.tb(*,chPlotArr(iChan)) LT maxBT_Values[chPlotArr(ichan)] $
                           AND refRadSim.lat GE MIN_LAT                $
                           AND refRadSim.lat LE MAX_LAT                $
                           AND refRadSim.tb(*,chPlotArr(iChan)) GT minBT_Values[chPlotArr(ichan)] $
                           AND refRadSim.tb(*,chPlotArr(iChan)) LT maxBT_Values[chPlotArr(ichan)] $
                           AND sfctype eq 0 $
                           AND refRadSim.nodeflag eq 1, nAlld )
      ; no good data continue 
      IF (nAlld eq 0) THEN continue 
      x1 = REFORM(refRadObs.tbdiff[filter_desc,chPlotArr[ichan]])
      ro = REFORM(refRadObs.tb[filter_desc,chPlotArr[ichan]])
      rs = REFORM(refRadSim.tb[filter_desc,chPlotArr[ichan]])

      ; compute stats
      stats.bias_desc[ichan] = MEAN(x1)
      stats.sdv_desc[ichan]  = sQRT(VARIANCE(x1))
      stats.rms_desc[ichan]  = SQRT(stats.bias_desc[ichan]^2. + stats.sdv_desc[ichan]^2.)
      stats.correl_desc[ichan] = CORRELATE(ro,rs)
      stats.n_desc[ichan] = nAlld
    ENDFOR

    RETURN

      
END
