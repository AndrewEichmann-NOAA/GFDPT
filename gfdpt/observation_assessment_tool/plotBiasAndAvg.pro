;---------------------------------------------------------------------------------
; Name:  plotBiasAndAvg.pro
;
; Type:  IDL Program
;
; Description:
;   A local procedure used by main-level assessment tool code
;   to plot radiance bias and standard deviation. 
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Apr 3, 2014, DXu, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO plotBiasAndAvg, sensorName, biasFileList, stddevFileList
   ; Float fill value 
   FLOAT_FILL_Value = -999.99

   ; Get number of bias files 
   nFiles = N_ELEMENTS(biasFileList)
   freq_unit = 'GHz'
   ; Loop thru. all the bias files 
   FOR iFile=0,nFiles-1 DO BEGIN
      ; Read bias file and get bias info per scan positions.
      Readbias, biasFileList(iFile), nChan, nPos, cFreq,  $
	 outMeanbias, outSlope, outIntercept, outTB_Sim, outTB_Meas

      ; Allocate space for all the bias files
      ; Last element in 3rd dimension holding average values. 
      IF (iFile EQ 0) THEN BEGIN
	 bias      = MAKE_ARRAY(nPos, nChan, nFiles + 1, VALUE = FLOAT_FILL_Value, /FLOAT)
	 slope     = MAKE_ARRAY(nPos, nChan, nFiles + 1, VALUE = FLOAT_FILL_Value, /FLOAT)
	 intercept = MAKE_ARRAY(nPos, nChan, nFiles + 1, VALUE = FLOAT_FILL_Value, /FLOAT)
	 TB_Sim    = MAKE_ARRAY(nPos, nChan, nFiles + 1, VALUE = FLOAT_FILL_Value, /FLOAT)
	 TB_Meas   = MAKE_ARRAY(nPos, nChan, nFiles + 1, VALUE = FLOAT_FILL_Value, /FLOAT)
	 scanPos = INDGEN(nPos) + 1
      ENDIF

      ; Save the current data when iFile = 0
      bias(*,*,iFile)      = outMeanbias
      slope(*,*,iFile)     = outSlope
      intercept(*,*,iFile) = outIntercept
      TB_Sim(*,*,iFile)    = outTB_Sim
      TB_Meas(*,*,iFile)   = outTB_Meas

      ;---compute averages : last element (nFiles) in last dimension. 
      ; Initialize sum to 0
      bias(*,*,nFiles)      = 0
      slope(*,*,nFiles)     = 0
      intercept(*,*,nFiles) = 0
      TB_Sim(*,*,nFiles)     = 0
      TB_Meas(*,*,nFiles)    = 0

      ; Average over the number of bias files 
      bias(*,*,nFiles)      = bias(*,*,nFiles) + bias(*,*,iFile) / nFiles
      slope(*,*,nFiles)     = slope(*,*,nFiles) + slope(*,*,iFile) / nFiles
      intercept(*,*,nFiles) = intercept(*,*,nFiles) + intercept(*,*,iFile) / nFiles
      TB_Sim(*,*,nFiles)    = TB_Sim(*,*,nFiles) + TB_Sim(*,*,iFile) / nFiles
      TB_Meas(*,*,nFiles)   = TB_Meas(*,*,nFiles) + TB_Meas(*,*,iFile) / nFiles

   ENDFOR 

   CLOSE, /ALL

   ; Read bias standard deviation 
   readStddev_SSMIS, nChan, nFiles, stddevFileList, stddevArr

   ; Set XSIZE and YSIZE for PS.
   xSizeVal=20
   ySizeVal=15

   ; Save graphics in PS
   SET_PLOT, 'PS'
   LOADCT, 39
   !P.MULTI = [0,1,1]
   !P.FONT = 2

   ;-----------------------------------------------
   ; Plot bias vs scan position for each channel
   ;-----------------------------------------------
   prefix = sensorName + "_bias_"
   ; Loop thru. channel to plot biases vs scan position
   FOR iChan = 0, nChan - 1 DO BEGIN
      ERASE
      imageName = prefix + strtrim(string(iChan + 1), 2) + '.ps'
      DEVICE, FILENAME = imageName, /COLOR, BITS_PER_PIXEL=8,            $
	  XSIZE = xSizeVal, YSIZE = ySizeVal, XOFFSET = 2, YOFFSET = 2,  $
	  /PORTRAIT, FONT_SIZE = 11, /BOLD, /COURIER
      titleName = STRCOMPRESS(sensorName + ' Mean Bias : Ch ' + STRING(iChan + 1) + ' (' $
		  + STRING(cFreq(iChan)) + ' ' + freq_unit + ')')
      PLOT, indgen(nPos) + 1, bias(*, iChan, nFiles), PSYM = -1, $
	 TITLE = titleName, $
	 YTITLE = 'Mean bias', XTITLE='Scan Position',  $
	 YRANGE = [min(bias(*,iChan,nFiles)), max(bias(*,iChan,nFiles))],   $
	 XRANGE = [0, nPos + 1]
      DEVICE, /CLOSE
   ENDFOR


   ;-----------------------------------------------
   ; Plot bias stddev vs channel
   ;-----------------------------------------------
   imageName = sensorName + '_bias_stddev.ps'
   ERASE
   DEVICE, FILENAME=imageName, /COLOR, BITS_PER_PIXEL=8,     $
      XSIZE=xSizeVal, YSIZE=ySizeVal, XOFFSET=2, YOFFSET=2,  $
      /PORTRAIT, FONT_SIZE=9, /BOLD, /COURIER

   titleName = sensorName + ' stddev of BT diff'
   PLOT, indgen(nChan) + 1, stddevArr, PSYM=-2, $
	TITLE = titleName, $
	YTITLE = 'std.', XTITLE=' Channel number ',  $
	YRANGE = [min(stddevArr), max(stddevArr)] , $
	XRANGE = [0, nChan + 1]

   DEVICE, /CLOSE
END


;-----------------------------------------------------------------------
; TODO, reading statements are based on SSMIS std deviaiton file format.
; We need to have separate routines to read stddev files for different sensors. 
;-----------------------------------------------------------------------
PRO readStddev_SSMIS, nChan, nFiles, stddevFileList, stddevArr
   ; Define variables
   tmpLine=''
   tmpData = FINDGEN(nChan)
   stddevArr = MAKE_ARRAY(nChan, /FLOAT, VALUE=0.0)

   ; This reading format is based on SSMIS bias file format
   ; Loop thru. all the stddev files to compute average stddev
   ; Normally, there is only one file to loop.
   FOR iFile = 0, nFiles-1 DO BEGIN
      ; Read the  stddev file
      OPENR, iu, stddevFileList[iFile], /GET_LUN
      READF, iu, tmpLine
      READF, iu, tmpLine
      ; Read channel frequency info
      READF, iu, FORMAT='(10f10.3/10f10.3/4f10.3)', tmpData
      READF, iu, tmpLine
      ; Read stddev values for all channels  
      READF, iu, FORMAT='(10f10.3/10f10.3/4f10.3)', tmpData
      
      ; Compute averaged stddev for all the channels 
      stddevArr(*) = stddevArr(*) + tmpData(*) / nFiles
   ENDFOR 

   CLOSE, /ALL
END
