PRO ReadAMVList, AMVFiles, sceneamv

  iu1 = 10 & iu2 = 11 
  CLOSE, iu1, iu2
  nfiles = N_ELEMENTS(AMVFiles)
  PRINT, AMVFiles[0], FORMAT="('...Reading file: ',a)"
  ReadSceneAMVHdr, iu1, AMVFiles[0], amv1
  ReadSceneAMV, iu1, amv1
  IF (nfiles eq 1) THEN BEGIN
     sceneamv = amv1
     return
  ENDIF
  FOR ifl = 1, nfiles-1L DO BEGIN
    PRINT, AMVFiles[ifl], FORMAT="('...Reading file: ',a)"
    ReadSceneAMVHdr, iu2, AMVFiles[ifl], amv2
    ReadSceneAMV, iu2, amv2
    CLOSE, iu1, iu2
    Merge_AMV, amv1, amv2, sceneamv
    amv1 = sceneamv
 ENDFOR
  
  PRINT, FORMAT="('...Finished reading/merging files: ')"

END
