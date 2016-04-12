PROGRAM COAST

  USE Consts
  USE Misc
  
  implicit none
  
  !---INTRINSIC functions used in this module
  INTRINSIC :: ADJUSTL,COUNT,INDEX,INT,MAXVAL,MINVAL,PACK,REAL,SIZE,SQRT,TRIM,ALL,MOD,LEN_TRIM

  REAL :: alat, alon, alat0, alon0
  REAL :: dfov

  CHARACTER(LEN=254) :: Topogr=DEFAULT_VALUE_STR4
  CHARACTER(LEN=254) :: Scenefile=DEFAULT_VALUE_STR4
  REAL :: xalt, xcover
  INTEGER :: iSfcTypCRTM, iSfcTypOut, i 
  REAL    :: offset_lon(8), offset_lat(8)
  INTEGER :: iProf, nPrf, iuEDR, ierr
  REAL, PARAMETER :: s1=1.0
  REAL, PARAMETER :: s2=0.0
!  TYPE(Scene_type)                      :: Scene    

  Topogr='/disks/data086/emaddy/dat/coat3/data/static/Topography/topography.bin_sgi'

  SceneFile = 'coast_test.dat'
  dfov  = 1.50
  
  alat0 = 8.
  alon0 = -59.0

  alat = alat0
  alon = alon0
!  offset_lon = (/1., sqrt(2.), -1., sqrt(2.),  1., -sqrt(2.), -1.,-sqrt(2.)/)
!  offset_lat = (/1., sqrt(2.), -1., -sqrt(2).,-1., sqrt(2).,   1.,-sqrt(2.)/)

!  offset_lon = (/s1, s2, -s1, s1,  s1, -s2, -1.,-sqrt(2.)/)
!  offset_lat = (/s1, s1, -s1, -s2.,-s1, s1,  1.,-sqrt(2.)/)

  offset_lon = (/s1, s1,  s1,  s2, -s1, -s1, -s1, s2/)
  offset_lat = (/s1, s2, -s1, -s1, -s1,  s2,  s1, s1/)

  iuEDR = 10

  CLOSE(iuEDR)
  OPEN(UNIT=iuEDR,FORM="FORMATTED",FILE=SceneFile)
  Read(iuEDR,*) nprf

  print *, nprf
  !---Loop over the profiles within the file
  ProfilesLoop: DO iprof=1,nPrf
    Read(iuEDR,*) alat0, alon0
    alat  = alat0
    alon  = alon0
    call Read_topography(Topogr,alat,alon,xalt,iSfcTypOut,xcover,iSfcTypCRTM)
    IF (iSfcTypOut .ne. 0) THEN 
       print *, 0, iProf, alat, alon, iSfcTypCRTM, iSfcTypOut
       CYCLE ProfilesLoop
    ENDIF
    OffsetLoop: DO i = 1, 8
       alat = alat0 + offset_lat(i)*dfov
       IF (alat .gt. 90.) alat = 90.
       IF (alat .lt. -90.) alat = -90.
       alon = alon0 + offset_lon(i)*dfov
       IF (alon .gt. 360.) alon = 360.
       IF (alon .lt. -180.) alon = -180.
       call Read_topography(Topogr,alat,alon,xalt,iSfcTypOut,xcover,iSfcTypCRTM)
!       print *, i, iProf, alat, alon, iSfcTypCRTM, iSfcTypOut
       IF (iSfcTypOut .ne. 0) EXIT OffsetLoop
    ENDDO OffsetLoop
    
    print *, i, iProf, alat0, alon0, iSfcTypCRTM, iSfcTypOut

 ENDDO ProfilesLoop
 CLOSE(iuEDR)
   
END PROGRAM COAST
