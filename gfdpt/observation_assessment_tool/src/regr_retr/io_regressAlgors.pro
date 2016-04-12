
;===================================================================
;
; Name: io_regressAlgors
;
; Language: IDL
;
; Programming Unit: Module
;
; Subroutines contained:
;   - readRegressAlgor
;
; Description:
;   - This IDL module contains the subroutine to read in the 
;     regression coefficient files. Currently only code is to 
;     read in the main body (after the header has been read)
;     but code should be added to read in the header as well.
;     That aspect for now is done in the main calling program.
;
;
; Author:
;   - Kevin Garrett, RTi @ NOAA/NESDIS/STAR/JCSDA
;
; History:
;   - 4/25/2014: Added readRegressAlgor
;
;
;====================================================================


;===================================================================
;
; Name: readRegressAlgor
;
; Language: IDL
;
; Programming Unit: Procedure
;
; Subroutines called:
;   - None
;
; Description:
;   - This IDL Procedure contains the subroutine to read in the 
;     regression coefficient file main body (after the header has 
;     been read)
;
; Arguments/Type/Description
;     AlgoFile     / Input    / Path/filename of the coefficient file
;     iu           / Input    / file logical unit number for io
;     sfcTyp       / Input    / Surface type array (n surface types)
;     Algor        / Output   / Array to store coefficients (nAlgors,nElemts,nIndepVar,nAngleBins)
;     Labels       / Input    / Array of labels (nAlgors)
;     nElemts      / Output   / Number of layers for product
;     nIndepVar    / Output   / Number of independent variables used in training
;     TbIndxV      / Ouput    / Array to store channel index used for training
;     FormIndxV    / Input    / Array to store format of Tbs used for training (log/phys)
;     ialgo        / Input    / Index of algorithm (by surface type)
;     nchan        / I/O      / Number of sensor channels
;     nAngleBins   / Input    / Number of angle bins the training is stratified by
;     isCrossTrack / Input   / CrossTrack or Conical scanning instrument flag
;
;
; Author:
;   - Kevin Garrett, RTi @ NOAA/NESDIS/STAR/JCSDA
;
; History:
;   - 4/25/2014: Initial code
;
;
;====================================================================

PRO readRegressAlgor,AlgoFile,iu,sfcTyp,Algor,Labels,nElemts,nIndepVar,TbIndxV,FormIndxV,ialgo,$
                     nchan,nAngBins,isCrossTrack
  line=''
  label=''

  ;---Body
  FOR iAng=0,nAngBins-1 DO BEGIN
      readf,iu,format='(a)',line
      readf,iu,format='(60x,a30)',label
      Labels=label
      readf,iu,format='(60x,i4)',sfcT
      sfcTyp(ialgo)=sfcT
      readf,iu,format='(60x,i4)',binnum
      readf,iu,format='(60x,i4)',nEl
      nElemts=nEl
      readf,iu,format='(60x,i4)',nInd
      nIndepVar(ialgo)=nInd
      ;nChan=nInd
      ;if (isCrosstrack eq 1) then nChan=nInd-1
      readf,iu,format='(a)',line
      TBindx=intarr(nchan)
      readf,iu,format='(60x,100i3)',TBindx
      FormIndx=intarr(nchan)
      readf,iu,format='(60x,100i3)',FormIndx
      TbIndxV[ialgo,0:nchan-1] = TBindx[0:nchan-1]
      FormIndxV[ialgo,0:nchan-1] = FormIndx[0:nchan-1]
      stat=intarr(1)
      readf,iu,format='(60x,200i4)',stat
      mCorr=fltarr(1)
      readf,iu,format='(60x,200f12.4)',mCorr  
      reg=fltarr(nIndepVar[ialgo]+1)
      FOR ivar=0,nElemts-1 DO BEGIN
          readf,iu,format='(20x,i4)',ivar0
          readf,iu,format='(12x,30f12.5)',reg
          readf,iu,format='(a)',line
          Algor[ialgo,ivar,0:nIndepVar[ialgo],iAng]=reg
      ENDFOR
  ENDFOR
END
