@tag_exist.pro 
PRO Create_Tabular_Summary, stats_clearsky, stats_unfiltered, stats_precip, stats_cloudy, tabular_info
;===============================================================
;+
; NAME: Create_Tabular_Summary
;
; Type:       IDL Subroutine
;
; Description: Create table LaTeX output for DAR report
;
; Arguments:
;      Name      Dim      Type             Description
;      ---------------------------------------------------
;      -stats_clearsky      Input            stats for clear sky
;      -stats_unfiltered    Input            stats for unfiltered
;      -stats_precip        Input            stats for precip
;      -stats_cloudy        Input            stats for cloudy cases
;      -tabular_info        Output           structure containing info
;                                            for LaTex tabular output
;
; History:  
;        07-18-2014    Eric S. Maddy RTi @ JCSDA Eric.Maddy@noaa.gov
;        10-20-2014    Mohar C Added codes to create Cloudy/Precip DAR
;        10-24-2014    ESM added frequency info to tabular output
;        10-29-2014    ESMaddy, turn off tabular info for zero cases
;        using flg_xxx[0,1,2] = [all, asc, desc]
;-
;===============================================================

columns = ['Chan','Freq, [' + stats_unfiltered.freq_unit + ']','Bias, [K]','Std. Dev., [K]', 'RMSE, [K]',$
           'Corr.', 'No of Cases']
type    = 'All Cases (unfiltered)'

nchan   = N_ELEMENTS(stats_clearsky.chan)
ncol    = N_ELEMENTS(columns)
;----strings for tabular output 
tex_unfiltered = STRARR(nchan,3)
tex_clearsky   = STRARR(nchan,3)
tex_cloudy     = STRARR(nchan,3)
tex_precip     = STRARR(nchan,3)
;----default turn on tabular summaries
flg_unfiltered = LONARR(3)+1
flg_clearsky   = LONARR(3)+1
flg_cloudy     = LONARR(3)+1
flg_precip     = LONARR(3)+1
;----loop over channels, create tabular LaTeX strings for all, ascending and descending orbits
FOR ich = 0L, nchan-1L DO BEGIN

;;; Unfiltered ;;;;

   str = STRTRIM(STRING(stats_unfiltered.chan[ich],"(i4)"),2) + '&' +$
         STRTRIM(STRING(stats_unfiltered.chanfreq[ich],"(a)"),2) + '&' +$
         STRTRIM(STRING(stats_unfiltered.bias_all[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_unfiltered.sdv_all[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_unfiltered.rms_all[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_unfiltered.correl_all[ich],"(f10.5)"),2) + '&' +$
         STRTRIM(STRING(stats_unfiltered.n_all[ich],"(i11)"),2) + '\\'
   
   tex_unfiltered[ich,0] = str
   tot = TOTAL(stats_unfiltered.n_all)
   IF (tot eq 0) THEN flg_unfiltered[0] = 0

   str = STRTRIM(STRING(stats_unfiltered.chan[ich],"(i4)"),2) + '&' +$
         STRTRIM(STRING(stats_unfiltered.chanfreq[ich],"(a)"),2) + '&' +$
         STRTRIM(STRING(stats_unfiltered.bias_asc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_unfiltered.sdv_asc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_unfiltered.rms_asc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_unfiltered.correl_asc[ich],"(f10.5)"),2) + '&' +$
         STRTRIM(STRING(stats_unfiltered.n_asc[ich],"(i11)"),2) + '\\'
   
   tex_unfiltered[ich,1] = str
   tot = TOTAL(stats_unfiltered.n_asc)
   IF (tot eq 0) THEN flg_unfiltered[1] = 0
   str = STRTRIM(STRING(stats_unfiltered.chan[ich],"(i4)"),2) + '&' +$
         STRTRIM(STRING(stats_unfiltered.chanfreq[ich],"(a)"),2) + '&' +$
         STRTRIM(STRING(stats_unfiltered.bias_desc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_unfiltered.sdv_desc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_unfiltered.rms_desc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_unfiltered.correl_desc[ich],"(f10.5)"),2) + '&' +$
         STRTRIM(STRING(stats_unfiltered.n_desc[ich],"(i11)"),2) + '\\'
   
   tex_unfiltered[ich,2] = str
   tot = TOTAL(stats_unfiltered.n_desc)
   IF (tot eq 0) THEN flg_unfiltered[2] = 0

;;; Clear Sky ;;;

   str = STRTRIM(STRING(stats_clearsky.chan[ich],"(i4)"),2) + '&' +$
         STRTRIM(STRING(stats_clearsky.chanfreq[ich],"(a)"),2) + '&' +$
         STRTRIM(STRING(stats_clearsky.bias_all[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_clearsky.sdv_all[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_clearsky.rms_all[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_clearsky.correl_all[ich],"(f10.5)"),2) + '&' +$
         STRTRIM(STRING(stats_clearsky.n_all[ich],"(i11)"),2) + '\\'
   
   tex_clearsky[ich,0] = str
   tot = TOTAL(stats_clearsky.n_all)
   IF (tot eq 0) THEN flg_clearsky[0] = 0

   str = STRTRIM(STRING(stats_clearsky.chan[ich],"(i4)"),2) + '&' +$
         STRTRIM(stats_clearsky.chanfreq[ich],2) + '&' +$
         STRTRIM(STRING(stats_clearsky.bias_asc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_clearsky.sdv_asc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_clearsky.rms_asc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_clearsky.correl_asc[ich],"(f10.5)"),2) + '&' +$
         STRTRIM(STRING(stats_clearsky.n_asc[ich],"(i11)"),2) + '\\'
   
   tex_clearsky[ich,1] = str
   tot = TOTAL(stats_clearsky.n_asc)
   IF (tot eq 0) THEN flg_clearsky[1] = 0
   str = STRTRIM(STRING(stats_clearsky.chan[ich],"(i4)"),2) + '&' +$
         STRTRIM(stats_clearsky.chanfreq[ich],2) + '&' +$
         STRTRIM(STRING(stats_clearsky.bias_desc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_clearsky.sdv_desc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_clearsky.rms_desc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_clearsky.correl_desc[ich],"(f10.5)"),2) + '&' +$
         STRTRIM(STRING(stats_clearsky.n_desc[ich],"(i11)"),2) + '\\'
   
   tex_clearsky[ich,2] = str
   tot = TOTAL(stats_clearsky.n_desc)
   IF (tot eq 0) THEN flg_clearsky[2] = 0

;;; Cloudy Sky ;;;

   str = STRTRIM(STRING(stats_cloudy.chan[ich],"(i4)"),2) + '&' +$
         STRTRIM(STRING(stats_cloudy.chanfreq[ich],"(a)"),2) + '&' +$
         STRTRIM(STRING(stats_cloudy.bias_all[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_cloudy.sdv_all[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_cloudy.rms_all[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_cloudy.correl_all[ich],"(f10.5)"),2) + '&' +$
         STRTRIM(STRING(stats_cloudy.n_all[ich],"(i11)"),2) + '\\'
   
   tex_cloudy[ich,0] = str
   tot = TOTAL(stats_cloudy.n_all)
   IF (tot eq 0) THEN flg_cloudy[0] = 0

   str = STRTRIM(STRING(stats_cloudy.chan[ich],"(i4)"),2) + '&' +$
         STRTRIM(STRING(stats_cloudy.chanfreq[ich],"(a)"),2) + '&' +$
         STRTRIM(STRING(stats_cloudy.bias_asc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_cloudy.sdv_asc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_cloudy.rms_asc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_cloudy.correl_asc[ich],"(f10.5)"),2) + '&' +$
         STRTRIM(STRING(stats_cloudy.n_asc[ich],"(i11)"),2) + '\\'
   
   tex_cloudy[ich,1] = str
   tot = TOTAL(stats_cloudy.n_asc)
   IF (tot eq 0) THEN flg_cloudy[1] = 0
   str = STRTRIM(STRING(stats_cloudy.chan[ich],"(i4)"),2) + '&' +$
         STRTRIM(STRING(stats_cloudy.chanfreq[ich],"(a)"),2) + '&' +$
         STRTRIM(STRING(stats_cloudy.bias_desc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_cloudy.sdv_desc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_cloudy.rms_desc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_cloudy.correl_desc[ich],"(f10.5)"),2) + '&' +$
         STRTRIM(STRING(stats_cloudy.n_desc[ich],"(i11)"),2) + '\\'
   
   tex_cloudy[ich,2] = str
   tot = TOTAL(stats_cloudy.n_desc)
   IF (tot eq 0) THEN flg_cloudy[2] = 0

;;; Precip sky ;;;
   str = STRTRIM(STRING(stats_precip.chan[ich],"(i4)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.chanfreq[ich],"(a)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.bias_all[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.sdv_all[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.rms_all[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.correl_all[ich],"(f10.5)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.n_all[ich],"(i11)"),2) + '\\'
   
   tex_precip[ich,0] = str
   tot = TOTAL(stats_precip.n_all)
   IF (tot eq 0) THEN flg_precip[0] = 0

   str = STRTRIM(STRING(stats_precip.chan[ich],"(i4)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.chanfreq[ich],"(a)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.bias_asc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.sdv_asc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.rms_asc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.correl_asc[ich],"(f10.5)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.n_asc[ich],"(i11)"),2) + '\\'
   
   tex_precip[ich,1] = str
   tot = TOTAL(stats_precip.n_asc)
   IF (tot eq 0) THEN flg_precip[1] = 0
   str = STRTRIM(STRING(stats_precip.chan[ich],"(i4)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.chanfreq[ich],"(a)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.bias_desc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.sdv_desc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.rms_desc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.correl_desc[ich],"(f10.5)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.n_desc[ich],"(i11)"),2) + '\\'
   
   tex_precip[ich,2] = str
   tot = TOTAL(stats_precip.n_desc)
   IF (tot eq 0) THEN flg_precip[2] = 0

;;; Ice sky ;;;
   str = STRTRIM(STRING(stats_precip.chan[ich],"(i4)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.chanfreq[ich],"(a)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.bias_all[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.sdv_all[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.rms_all[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.correl_all[ich],"(f10.5)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.n_all[ich],"(i11)"),2) + '\\'
   
   tex_precip[ich,0] = str
   tot = TOTAL(stats_precip.n_all)
   IF (tot eq 0) THEN flg_precip[0] = 0
   str = STRTRIM(STRING(stats_precip.chan[ich],"(i4)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.chanfreq[ich],"(a)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.bias_asc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.sdv_asc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.rms_asc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.correl_asc[ich],"(f10.5)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.n_asc[ich],"(i11)"),2) + '\\'
   
   tex_precip[ich,1] = str
   tot = TOTAL(stats_precip.n_asc)
   IF (tot eq 0) THEN flg_precip[1] = 0

   str = STRTRIM(STRING(stats_precip.chan[ich],"(i4)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.chanfreq[ich],"(a)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.bias_desc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.sdv_desc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.rms_desc[ich],"(f7.2)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.correl_desc[ich],"(f10.5)"),2) + '&' +$
         STRTRIM(STRING(stats_precip.n_desc[ich],"(i11)"),2) + '\\'
   
   tex_precip[ich,2] = str
   tot = TOTAL(stats_precip.n_desc)
   IF (tot eq 0) THEN flg_precip[2] = 0

ENDFOR

cstr = columns[0] + ' &' + columns[1] + '&' + columns[2] + '&' + $
       columns[3] + ' & '+ columns[4] + '&' + columns[5] + '&' + $
       columns[6] + '\\'
;cstr = columns[0] + ' &' + columns[1] + '&' + columns[2] + '&' + $
;       columns[3] + ' & '+ columns[4] + '&' + columns[5] + '\\'

IF (nchan ge 14) THEN size='\tiny' ELSE size='\scriptsize'
header  = [size, $
           '\begin{center}', $
           '\begin{tabular}{ccccccc}', $
           '\multicolumn{7}{c}{', $
           cstr, $
           '\hline', $
           '\hline']
           
tabular_info = {header: header, tex_unfiltered: tex_unfiltered, $
                tex_clearsky: tex_clearsky, tex_cloudy: tex_cloudy, $
                tex_precip: tex_precip, flg_unfiltered: flg_unfiltered, $
                flg_clearsky: flg_clearsky, flg_cloudy: flg_cloudy, $
                flg_precip: flg_precip}

END
PRO CreateCaptions, imageFiles, sensorName, date, captions, chnum=chnumx
;===============================================================
;+
; NAME: CreateCaptions
;
; Type:       IDL Subroutine
;
; Description: Create captions for DAR figures
;
; Arguments:
;      Name      Dim      Type             Description
;      ---------------------------------------------------
;      -imageFiles      Input            name of figures to include
;      -sensorName      Input            name of sensor
;      -date            Input            date
;      -chnum           Input            channel number
;      -captions        Output           figure captions for DAR
;
; History:  
;        07-08-2014    Eric S. Maddy RTi @ JCSDA Eric.Maddy@noaa.gov
;        10-27-2014    ESMaddy added RWP, GWP, WVXXX hPa tokens
;        01-21-2015    ESMaddy added emissivity tokens (model - analytic)
;        01-27-2015    ESMaddy file -> file_basename to remove any
;        dependence of tokens on image directory name.
;-
;===============================================================

;---tokens for ascending and descending node if applicable
tokens_node = ['asc','desc']
nnode       = N_ELEMENTS(tokens_node)
desc_node   = ['ascending node', 'descending node']

;---tokens for type of scatter plot (bias and bias_vs need own category) 
tokens_scat = ['Rad', 'TPW', 'Lat', 'SkinT', 'ScanPos', 'bias', $
               'RWP', 'GWP', 'WV950', 'WV500', 'WV300', 'emisdiff']
desc_scat   = ['observed vs simulated BT', $
               'obs - sim BT versus TPW', 'obs - sim BT versus latitude', $
               'obs - sim BT versus skin temperature', $
               'obs - sim BT versus scan position','obs - sim BT bias',$
               'obs - sim BT versus RWP', 'obs - sim BT versus GWP', $
               'obs - sim BT versus 950hPa WV', $
               'obs - sim BT versus 500hPa WV', $
               'obs - sim BT versus 300hPa WV', $
               'obs - sim BT versus emis difference']
nscat       = N_ELEMENTS(tokens_scat)

;---tokens for type of plot 
tokens_type = ['Rad_plot','ScatPlot']
desc_type   = ['observed BT, simulated BT, and BT difference',$
               'density scatter plot of','plot of']
ntype       = N_ELEMENTS(tokens_type)

;---tokens for clear, cloudy, all-sky, etc.
tokens_sky  = ['', 'ClrSky']
desc_sky    = ['all sky','clear sky']
nsky        = N_ELEMENTS(tokens_sky)

;---tokens for surface type
tokens_sfc  = ['ocean', 'land','seaice','snow']
desc_sfc    = ['ocean only', 'land only', 'seaice only', 'snow only']
nsfc        = N_ELEMENTS(tokens_sfc)

nfl         = N_ELEMENTS(imageFiles)
captions    = STRARR(nfl)
;---remove underscore from sensor name if applicable 
sensorNameu = STRJOIN(STRSPLIT(sensorName,'_',/EXTRACT),' ')

;---see if channel numbers were passed.  if so, use them.
chstr       = captions
IF (KEYWORD_SET(chnumx)) THEN chstr = ' channel ' + chnumx

;---loop over input files and parse the name to create captions
FOR ifl = 0L, nfl-1L DO BEGIN

  ;---default most to empty strings
  descnode = ''
  descscat = ''
  desctype = ''
  descsfc  = ''
  descsky  = desc_sky[0]
  ;---current file 
  file = FILE_BASENAME(imageFiles[ifl])

  ;---determine if file is for asc or desc or all
  FOR inode = 0L, nnode-1L DO BEGIN
    ipos = STRPOS(file,tokens_node[inode])
    IF (ipos ne -1) THEN descnode = desc_node[inode]
  ENDFOR

  ;---determine if file is rad map or scatter plot 
  FOR itype = 0L, ntype-1L DO BEGIN
    ipos = STRPOS(file,tokens_type[itype])
    IF (ipos ne -1) THEN desctype = desc_type[itype]
    ;---if it is a scatter type then determine which type
    IF (itype eq 1 and ipos ne -1) THEN BEGIN
      FOR iscat = 0L, nscat-1L DO BEGIN
         ipos = STRPOS(file,tokens_scat[iscat])
         IF (ipos ne -1) THEN descscat = ' ' + desc_scat[iscat]
         IF (ipos ne -1 and tokens_scat[iscat] eq 'bias') THEN $
            desctype = ' ' + desc_type[itype+1] 
      ENDFOR
    ENDIF
  ENDFOR
 
 ;---determine if file is all sky or clear sky or ...
  FOR isky = 0L, nsky-1L DO BEGIN
    ipos = STRPOS(file,tokens_sky[isky])
    IF (ipos ne -1) THEN descsky = desc_sky[isky]
  ENDFOR

  ;---determine if file is ocean only, land only, etc.
  FOR isfc = 0L, nsfc-1L DO BEGIN
    ipos = STRPOS(file,tokens_sfc[isfc])
    IF (ipos ne -1) THEN descsfc = ', ' + desc_sfc[isfc]
  ENDFOR

  ;---build caption string
  captions[ifl] = sensorNameu + chstr[ifl] + ' ' + descsky + descsfc + ' ' + $
                  desctype + descscat   
  IF (descnode ne '') THEN captions[ifl] = captions[ifl] + ' for ' + $
                                          descnode 
ENDFOR ;---loop over files 
  
END
PRO DAR_setupandrun, inputConfig 
;===============================================================
;+
; NAME: DAR_setupandrun
;
; Type:       IDL Subroutine
;
; Description: Setup DAR information based on COAT run
;
; Arguments:
;      Name      Dim      Type             Description
;      ---------------------------------------------------
;      -inputConfig      Input            Structure containing output
;      information for the Data Assessment Report
;
; History:  
;        07-08-2014    Eric S. Maddy RTi @ JCSDA Eric.Maddy@noaa.gov
;
;-
;===============================================================

 ;---Get input config params
; chanNumArr = inputConfig.chanNumArr
; chanInfoArr = inputConfig.chanInfoArr
; prefix  = inputConfig.prefix
; date = inputConfig.date
 imageDir = inputConfig.imageDir

;---channel dependent statistics
;   (channel number, bias, standard
;   deviation, rms, correlation, number of obs. for clear and all sky
;   cases)
 stats_clearSky   = inputConfig.stats_ClearSky
 stats_Unfiltered = inputConfig.stats_Unfiltered
 stats_cloudy = inputConfig.stats_Cloudy
 stats_precip = inputConfig.stats_Precip
 ;---generate tabular output summaries of statistics 
 Create_Tabular_Summary, stats_clearsky, stats_unfiltered, $
                         stats_precip, stats_cloudy, tabular_info
 
 ;---Get sensor name 
 sensorName = inputConfig.sensorName
 filesRad   = FILE_SEARCH(imageDir + '/' + sensorName + '_Rad_plot*.eps')
 ;---need to be choosy with these files
 filesScat  = FILE_SEARCH(imageDir + '/' + sensorName + '_ScatPlot*ocean*.eps')

 ;---scatterplots are organized by channel number
 ;---sort channel numbers and figures by channel index
 chnum   = LONARR(N_ELEMENTS(filesScat))
 FOR ifl = 0L, N_ELEMENTS(filesScat)-1L DO BEGIN
    file   = filesScat[ifl]
    ipos   = STRPOS(file,'ch')
    strspl = STRSPLIT(STRMID(file,ipos+2),'_',/EXTRACT)
    chnum[ifl] = LONG(strspl[0])
 ENDFOR
 isort = SORT(chnum)
 filesScat = filesScat[isort]
 chnum = STRTRIM(STRING(chnum[isort],"(i4)"),2)

 ;---output Maps to one file 
 CreateCaptions, filesRad, sensorName, date, captions
 DAR_config = {imageFiles: filesRad, $ 
               captions: captions,$
               filename: sensorName + '_DAR_maps.tex'}
 Create_DataAssessmentReport, DAR_config

 ;---output other figures to another file 
 CreateCaptions, filesScat, sensorName, date, captions, chnum=chnum
 DAR_config = {imageFiles: filesScat, $
               captions: captions,$
               sensorname: STRJOIN(STRSPLIT(sensorName,'_',/EXTRACT),' '), $
               filename: sensorName + '_DAR_stratified.tex', $
               tabular_info: tabular_info}
 Create_DataAssessmentReport, DAR_config

END
PRO Create_DataAssessmentReport, DAR_config
;=========================================================
;+
; NAME: Create_DataAssessmentReport.pro
;
; Type:       IDL Subroutine
;
; Description: Create Data Assessment Report from the COAT
;
; Arguments:
;      Name      Dim      Type             Description
;      ---------------------------------------------------
;      -DAR_config      Input            Structure containing output
;      information for the Data Assessment Report
;
; History:  
;        07-07-2014    Eric S. Maddy RTi @ JCSDA Eric.Maddy@noaa.gov
;        10-15-2014    ESM added multiple frames for tabular output if 
;                      number of channels > 25
;        10-29-2014    ESMaddy, turn off tabular info for zero cases
;        using flg_xxx[0,1,2] = [all, asc, desc]
;
; Requires: pdflatex installed on machine with beamer 
;           and other packages ... see preamble below
;
;-
;===============================================================

;---get configurable information
DARfilename = DAR_config.filename
captions    = DAR_config.captions
imageFiles  = DAR_config.imageFiles
nfl         = N_ELEMENTS(imageFiles)

;---open output file for the Data Assessment Report
OPENW, lunx, DARfilename, /GET_LUN

;---write preamble 
PRINTF, lunx, "\documentclass[xcolor=pdftex,dvipsnames,table,hyperref={pdfpagelabels=false},handout]{beamer}"
PRINTF, lunx, "\usepackage{xspace}"
PRINTF, lunx, "\usepackage{epstopdf}"
PRINTF, lunx, "\usepackage{multirow}"
PRINTF, lunx, "\usepackage{textpos}"
PRINTF, lunx, "\usepackage{pdfpages}"
PRINTF, lunx, "\usepackage{bm}"
PRINTF, lunx, "\setbeamercovered{transparent}"
PRINTF, lunx, "\usepackage[latin1]{inputenc}"
PRINTF, lunx, "\usefonttheme{professionalfonts}"
PRINTF, lunx, "\usepackage{times}"
PRINTF, lunx, "\usepackage{amsmath}"
PRINTF, lunx, "\usepackage{verbatim}"

;---create newcommand for figures
PRINTF, lunx, "\newcommand{\landgraph}[3][1]"
PRINTF, lunx, "   {"
PRINTF, lunx, "   \label{#2}"
PRINTF, lunx, "   \begin{center}"
PRINTF, lunx, "   \includegraphics[width=#1\textwidth,height=#1\textheight]{#3}   %,height=!"
PRINTF, lunx, "   \end{center}"
PRINTF, lunx, "   }"

;---create document
PRINTF, lunx, "\begin{document}"

;---check for the existence of tabular_info structure tag
;   if it exists then write tabular info to DAR file.
IF (TAG_EXIST(DAR_config,'tabular_info')) THEN BEGIN
  ;---create slide for unfiltered
  nhead = N_ELEMENTS(DAR_config.tabular_info.header)
  header = DAR_config.tabular_info.header
  ;---get number of frames required for output 
  ; - Hyperspectral IR has 10x or more the number of 
  ;   channels.  25 maximum number of channels per page
  i1 = SIZE(DAR_config.tabular_info.tex_unfiltered,/DIMENSION)
  nchan = i1[0]
  nchan_pp_max = 25
  IF (nchan gt nchan_pp_max) THEN $
     nframe = CEIL(FLOAT(nchan)/FLOAT(nchan_pp_max)) ELSE $
        nframe = 1
  chstart = 0L
  ; all unfiltered cases
  FOR iframe = 0L, nframe-1L DO BEGIN
    if (DAR_config.tabular_info.flg_unfiltered[0] eq 0) THEN continue 
    cst = chstart
    csp = chstart+nchan_pp_max
    csp = MIN([csp,nchan])
    ndx = csp-cst+1
    IF (cst ge nchan) THEN continue 
    PRINTF, lunx, '\begin{frame}'
    FOR ih = 0L, nhead-1L DO BEGIN
       head = header[ih]
       IF (ih eq 3) THEN head = header[ih] + DAR_config.sensorName + ' ' +$
                                'All Unfiltered Cases (Ocean Only)} \\'
       PRINTF, lunx, head
    ENDFOR
    FOR ich = cst, csp-1L DO $
       PRINTF, lunx, DAR_config.tabular_info.tex_unfiltered[ich,0]
    
    PRINTF, lunx, '\end{tabular}'
    PRINTF, lunx, '\end{center}\end{frame}'
    ;---increment channel counters
    chstart = csp + 1
  ENDFOR
  ; unfileterd ascending
  chstart = 0L
  FOR iframe = 0L, nframe-1L DO BEGIN
    if (DAR_config.tabular_info.flg_unfiltered[1] eq 0) THEN continue 
    cst = chstart
    csp = chstart+nchan_pp_max
    csp = MIN([csp,nchan])
    ndx = csp-cst+1
    IF (cst ge nchan) THEN continue 
    PRINTF, lunx, '\begin{frame}'
    FOR ih = 0L, nhead-1L DO BEGIN
       head = header[ih]
       IF (ih eq 3) THEN head = header[ih] + DAR_config.sensorName + ' ' +$
                                'Ascending Unfiltered Cases (Ocean Only)} \\'
       PRINTF, lunx, head
    ENDFOR
    FOR ich = cst, csp-1L DO $
       PRINTF, lunx, DAR_config.tabular_info.tex_unfiltered[ich,1]
    PRINTF, lunx, '\end{tabular}'
    PRINTF, lunx, '\end{center}\end{frame}'
    ;---increment channel counters
    chstart = csp + 1
  ENDFOR
  ; unfiltered descending
  chstart = 0L
  FOR iframe = 0L, nframe-1L DO BEGIN
    if (DAR_config.tabular_info.flg_unfiltered[2] eq 0) THEN continue 
    cst = chstart
    csp = chstart+nchan_pp_max
    csp = MIN([csp,nchan])
    ndx = csp-cst+1
    IF (cst ge nchan) THEN continue 
    PRINTF, lunx, '\begin{frame}'
    FOR ih = 0L, nhead-1L DO BEGIN
       head = header[ih]
       IF (ih eq 3) THEN head = header[ih] + DAR_config.sensorName + ' ' +$
                                'Descending Unfiltered Cases (Ocean Only)} \\'
       PRINTF, lunx, head
    ENDFOR
    FOR ich = cst, csp-1L DO $
       PRINTF, lunx, DAR_config.tabular_info.tex_unfiltered[ich,2]
    PRINTF, lunx, '\end{tabular}'
    PRINTF, lunx, '\end{center}\end{frame}'
    ;---increment channel counters
    chstart = csp+1
  ENDFOR
  ; clear sky all
  chstart = 0L
  FOR iframe = 0L, nframe-1L DO BEGIN
    if (DAR_config.tabular_info.flg_clearsky[0] eq 0) THEN continue 
    cst = chstart
    csp = chstart+nchan_pp_max
    csp = MIN([csp,nchan])
    ndx = csp-cst+1
    IF (cst ge nchan) THEN continue 
    PRINTF, lunx, '\begin{frame}'
    FOR ih = 0L, nhead-1L DO BEGIN
       head = header[ih]
       IF (ih eq 3) THEN head = header[ih] + DAR_config.sensorName + ' ' +$
                                'All Clear-Sky Cases (Ocean Only)} \\ '
       PRINTF, lunx, head
    ENDFOR
    FOR ich = cst, csp-1L DO $
       PRINTF, lunx, DAR_config.tabular_info.tex_clearsky[ich,0]
    PRINTF, lunx, '\end{tabular}'
    PRINTF, lunx, '\end{center}\end{frame}'
    ;---increment channel counters
    chstart = csp+1
  ENDFOR
  ; clear sky ascending
  chstart = 0L
  FOR iframe = 0L, nframe-1L DO BEGIN
    if (DAR_config.tabular_info.flg_clearsky[1] eq 0) THEN continue 
    cst = chstart
    csp = chstart+nchan_pp_max
    csp = MIN([csp,nchan])
    ndx = csp-cst+1
    IF (cst ge nchan) THEN continue 
    PRINTF, lunx, '\begin{frame}'
    FOR ih = 0L, nhead-1L DO BEGIN
       head = header[ih]
       IF (ih eq 3) THEN head = header[ih] + DAR_config.sensorName + ' ' +$
                                'Ascending Clear-Sky Cases (Ocean Only)} \\'
       PRINTF, lunx, head
    ENDFOR
    FOR ich = cst, csp-1L DO $
        PRINTF, lunx, DAR_config.tabular_info.tex_clearsky[ich,1]
    PRINTF, lunx, '\end{tabular}'
    PRINTF, lunx, '\end{center}\end{frame}'
    ;---increment channel counters
    chstart = csp+1
  ENDFOR
  ; clear sky descending
  chstart = 0L
  FOR iframe = 0L, nframe-1L DO BEGIN
    if (DAR_config.tabular_info.flg_clearsky[2] eq 0) THEN continue 
    cst = chstart
    csp = chstart+nchan_pp_max
    csp = MIN([csp,nchan])
    ndx = csp-cst+1
    IF (cst ge nchan) THEN continue 
    PRINTF, lunx, '\begin{frame}'
    FOR ih = 0L, nhead-1L DO BEGIN
       head = header[ih]
       IF (ih eq 3) THEN head = header[ih] + DAR_config.sensorName + ' ' +$
                                'Descending Clear-Sky Cases (Ocean Only)} \\'
       PRINTF, lunx, head
    ENDFOR
    FOR ich = cst, csp-1L DO $
        PRINTF, lunx, DAR_config.tabular_info.tex_clearsky[ich,2]
    PRINTF, lunx, '\end{tabular}'
    PRINTF, lunx, '\end{center}\end{frame}'
    ;---increment channel counters
    chstart = csp+1
  ENDFOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; cloudy sky all
  chstart = 0L
  FOR iframe = 0L, nframe-1L DO BEGIN
    if (DAR_config.tabular_info.flg_cloudy[0] eq 0) THEN continue 
    cst = chstart
    csp = chstart+nchan_pp_max
    csp = MIN([csp,nchan])
    ndx = csp-cst+1
    IF (cst ge nchan) THEN continue 
    PRINTF, lunx, '\begin{frame}'
    FOR ih = 0L, nhead-1L DO BEGIN
       head = header[ih]
       IF (ih eq 3) THEN head = header[ih] + DAR_config.sensorName + ' ' +$
                                'All Cloudy-Sky Cases (Ocean Only)} \\ '
       PRINTF, lunx, head
    ENDFOR
    FOR ich = cst, csp-1L DO $
       PRINTF, lunx, DAR_config.tabular_info.tex_cloudy[ich,0]
    PRINTF, lunx, '\end{tabular}'
    PRINTF, lunx, '\end{center}\end{frame}'
    ;---increment channel counters
    chstart = csp+1
  ENDFOR
  ; cloudy sky ascending
  chstart = 0L
  FOR iframe = 0L, nframe-1L DO BEGIN
    if (DAR_config.tabular_info.flg_cloudy[1] eq 0) THEN continue 
    cst = chstart
    csp = chstart+nchan_pp_max
    csp = MIN([csp,nchan])
    ndx = csp-cst+1
    IF (cst ge nchan) THEN continue 
    PRINTF, lunx, '\begin{frame}'
    FOR ih = 0L, nhead-1L DO BEGIN
       head = header[ih]
       IF (ih eq 3) THEN head = header[ih] + DAR_config.sensorName + ' ' +$
                                'Ascending Cloudy-Sky Cases (Ocean Only)} \\'
       PRINTF, lunx, head
    ENDFOR
    FOR ich = cst, csp-1L DO $
        PRINTF, lunx, DAR_config.tabular_info.tex_cloudy[ich,1]
    PRINTF, lunx, '\end{tabular}'
    PRINTF, lunx, '\end{center}\end{frame}'
    ;---increment channel counters
    chstart = csp+1
  ENDFOR
  ; cloudy sky descending
  chstart = 0L
  FOR iframe = 0L, nframe-1L DO BEGIN
    if (DAR_config.tabular_info.flg_cloudy[2] eq 0) THEN continue 
    cst = chstart
    csp = chstart+nchan_pp_max
    csp = MIN([csp,nchan])
    ndx = csp-cst+1
    IF (cst ge nchan) THEN continue 
    PRINTF, lunx, '\begin{frame}'
    FOR ih = 0L, nhead-1L DO BEGIN
       head = header[ih]
       IF (ih eq 3) THEN head = header[ih] + DAR_config.sensorName + ' ' +$
                                'Descending Cloudy-Sky Cases (Ocean Only)} \\'
       PRINTF, lunx, head
    ENDFOR
    FOR ich = cst, csp-1L DO $
        PRINTF, lunx, DAR_config.tabular_info.tex_cloudy[ich,2]
    PRINTF, lunx, '\end{tabular}'
    PRINTF, lunx, '\end{center}\end{frame}'
    ;---increment channel counters
    chstart = csp+1
  ENDFOR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Precip all
  chstart = 0L
  FOR iframe = 0L, nframe-1L DO BEGIN
    if (DAR_config.tabular_info.flg_precip[0] eq 0) THEN continue 
    cst = chstart
    csp = chstart+nchan_pp_max
    csp = MIN([csp,nchan])
    ndx = csp-cst+1
    IF (cst ge nchan) THEN continue 
    PRINTF, lunx, '\begin{frame}'
    FOR ih = 0L, nhead-1L DO BEGIN
       head = header[ih]
       IF (ih eq 3) THEN head = header[ih] + DAR_config.sensorName + ' ' +$
                                'All Precip-Sky Cases (Ocean Only)} \\ '
       PRINTF, lunx, head
    ENDFOR
    FOR ich = cst, csp-1L DO $
       PRINTF, lunx, DAR_config.tabular_info.tex_precip[ich,0]
    PRINTF, lunx, '\end{tabular}'
    PRINTF, lunx, '\end{center}\end{frame}'
    ;---increment channel counters
    chstart = csp+1
  ENDFOR
  ; precip sky ascending
  chstart = 0L
  FOR iframe = 0L, nframe-1L DO BEGIN
    if (DAR_config.tabular_info.flg_precip[1] eq 0) THEN continue 
    cst = chstart
    csp = chstart+nchan_pp_max
    csp = MIN([csp,nchan])
    ndx = csp-cst+1
    IF (cst ge nchan) THEN continue 
    PRINTF, lunx, '\begin{frame}'
    FOR ih = 0L, nhead-1L DO BEGIN
       head = header[ih]
       IF (ih eq 3) THEN head = header[ih] + DAR_config.sensorName + ' ' +$
                                'Ascending Precip-Sky Cases (Ocean Only)} \\'
       PRINTF, lunx, head
    ENDFOR
    FOR ich = cst, csp-1L DO $
        PRINTF, lunx, DAR_config.tabular_info.tex_precip[ich,1]
    PRINTF, lunx, '\end{tabular}'
    PRINTF, lunx, '\end{center}\end{frame}'
    ;---increment channel counters
    chstart = csp+1
  ENDFOR
  ; precip sky descending

  chstart = 0L
  FOR iframe = 0L, nframe-1L DO BEGIN
    if (DAR_config.tabular_info.flg_precip[2] eq 0) THEN continue 
    cst = chstart
    csp = chstart+nchan_pp_max
    csp = MIN([csp,nchan])
    ndx = csp-cst+1
    IF (cst ge nchan) THEN continue 
    PRINTF, lunx, '\begin{frame}'
    FOR ih = 0L, nhead-1L DO BEGIN
       head = header[ih]
       IF (ih eq 3) THEN head = header[ih] + DAR_config.sensorName + ' ' +$
                                'Descending Precip-Sky Cases (Ocean Only)} \\'
       PRINTF, lunx, head
    ENDFOR
    FOR ich = cst, csp-1L DO $
        PRINTF, lunx, DAR_config.tabular_info.tex_precip[ich,2]
    PRINTF, lunx, '\end{tabular}'
    PRINTF, lunx, '\end{center}\end{frame}'
    ;---increment channel counters
    chstart = csp+1
  ENDFOR
 
ENDIF  ;---end of tabular optional output 

;---figure loop
FOR ifl = 0L, nfl-1L DO BEGIN 
  ipos = STRPOS(imageFiles[ifl],'.eps')
  ;---remove file extension
  fx = FILE_DIRNAME(imageFiles[ifl]) + '/' + $
       FILE_BASENAME(imageFiles[ifl],'.eps')
  ;---test if files have already been converted to pdf
  sfx = '.eps'
  IF (FILE_TEST(fx + '.pdf')) THEN sfx = '.pdf'

  lbl = 'fig_' + STRTRIM(STRING(ifl),2)
  ;---create slide
  PRINTF, lunx, '\begin{frame}'
  str = '\landgraph[0.85]{' + lbl + '}{{' + fx + '}' + sfx + '}{}'
  ;---output picture and place caption
  PRINTF, lunx, str
  PRINTF, lunx, '\scriptsize ' + captions[ifl]
  ;---end slide
  PRINTF, lunx, '\end{frame}'
ENDFOR  
;---end document and close file 
PRINTF, lunx, "\end{document}"
CLOSE, lunx 
FREE_LUN, lunx

PRINT, FORMAT="('...Creating Data Assessment Report...this may take a while.')"
cmd = 'pdflatex --shell-escape ' + DARfilename  
;---compile twice (first time creates pdf, second gets references correct).
SPAWN, cmd
cmd = 'pdflatex  ' + DARfilename  
SPAWN, cmd

END
