PROGRAM COAST

  USE Consts
  USE Misc
  USE IO_Scene
  USE utils
  
  implicit none
  
  !---INTRINSIC functions used in this module
  INTRINSIC :: ADJUSTL,COUNT,INDEX,INT,MAXVAL,MINVAL,PACK,REAL,SIZE,SQRT,TRIM,ALL,MOD,LEN_TRIM

  REAL :: alat, alon, alat0, alon0
  REAL :: dfov

  CHARACTER(LEN=254) :: Topogr=DEFAULT_VALUE_STR4
  CHARACTER(LEN=254) :: Scenefile=DEFAULT_VALUE_STR4
  REAL :: xalt, xcover
  INTEGER :: iSfcTypCRTM, iSfcTypOut, i 
  INTEGER :: offset_lon(4), offset_lat(4)
  INTEGER :: iProf, nPrf, iuEDR, ierr
  TYPE(Scene_type)                      :: Scene    

  Topogr='/data/users/emaddy/miidaps/data/StaticData/Topography/topography.bin_sgi'
  SceneFile = '/data/users/emaddy/miidaps/data/TestbedData/Outputs/edr/npp_crisatms_dummy_mw/2014-10-05CHPse_ec.cratm.t12z.cris.tm00.bufr_d.bin_LR_019'
  SceneFile='/data/users/emaddy/miidaps/data/TestbedData/DynamicData/nwp_analys/npp_cris_dummy/NWP_ECMW.t12z.cris.tm00.bufr_d.bin'  

  dfov  = 1.00
  
  alat0 = 8.
  alon0 = -59.0

  alat = alat0
  alon = alon0
  offset_lon = (/1, -1, 1, -1/)
  offset_lat = (/1, -1, -1, 1/)

  CALL ReadHdrScene(iuEDR,SceneFile,Scene,nprf)
  print *, nprf
  !---Loop over the profiles within the file
  ProfilesLoop: DO iprof=1,nPrf
    CALL ReadScene(iuEDR,Scene,ierr)
    alat0 = Scene%lat
    alon0 = Scene%lon
    alat  = alat0
    alon  = alon0
    call Read_topography(Topogr,alat,alon,xalt,iSfcTypOut,xcover,iSfcTypCRTM)
    IF (iSfcTypOut .ne. 0) THEN 
       print *, 0, iProf, alat, alon, iSfcTypCRTM, iSfcTypOut
       CYCLE ProfilesLoop
    ENDIF
    OffsetLoop: DO i = 1, 4
       alat = alat0 + FLOAT(offset_lat(i))*dfov
       IF (alat .gt. 90.) alat = 90.
       IF (alat .lt. -90.) alat = -90.
       alon = alon0 + FLOAT(offset_lon(i))*dfov
       IF (alon .gt. 360.) alon = 360.
       IF (alon .lt. -180.) alon = -180.
       call Read_topography(Topogr,alat,alon,xalt,iSfcTypOut,xcover,iSfcTypCRTM)
!       print *, i, iProf, alat, alon, iSfcTypCRTM, iSfcTypOut
       IF (iSfcTypOut .ne. 0) EXIT OffsetLoop
    ENDDO OffsetLoop
    
    print *, i, iProf, alat0, alon0, iSfcTypCRTM, iSfcTypOut

 ENDDO ProfilesLoop
  
END PROGRAM COAST
