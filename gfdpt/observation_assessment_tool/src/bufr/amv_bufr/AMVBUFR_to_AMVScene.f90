PROGRAM AMVBUFR_to_AMVScene
!$Id: Test.f90 2782 2014-04-04 06:00:59Z emaddy $
!-----------------------------------------------------------------------------------------------
! Name:         AMVBUFR_to_AMVScene
! 
! Type:         F90 main program
!
! Description:
!       This routine converts BUFRized satellite wind data to SceneAMV_type output 
!
! Modules needed:
!       - Consts
!       - ErrorHandling
!       - SceneAMV_type
!       - BUFR
!
! Subroutines contained:
!       None
!
! Data type included:
!       - SceneAMV_type
!
! 
! History:
!       04-03-2014      Eric S. Maddy RTi @ JCSDA  Created 
!       04-04-2014      ESM                        Added Quality Markers    
!       04-07-2014      ESM                        Added Namelist Driver - cleaned up code
!-----------------------------------------------------------------------------------------------

  USE Consts
  USE IO_Misc
  USE IO_SceneAMV
  USE ErrorHandling

  IMPLICIT NONE

  !---INTRINSIC functions used in this module
  INTRINSIC :: ADJUSTL,COUNT,INDEX,INT,MAXVAL,MINVAL,PACK,REAL,SIZE,SQRT,TRIM,ALL,MOD,LEN_TRIM

  !---local variables
  INTEGER                               :: nqc, nProfiles
  REAL(8),DIMENSION(13)                 :: hdrdat
  REAL(8),DIMENSION(4)                  :: obsdat
  REAL(8),DIMENSION(4)                  :: satqc
  REAL(8),DIMENSION(3,12)               :: qcdat
  INTEGER                               :: iret
  CHARACTER(70)                         :: obstr,hdrtr
  CHARACTER(50)                         :: satqctr,qcstr
  CHARACTER(8)                          :: subset
  CHARACTER(20)                         :: derdwtr,heightr
  CHARACTER(8)                          :: c_prvstg,c_sprvstg
  CHARACTER(8)                          :: c_station_id
  CHARACTER(256)                        :: infile, outfile
  INTEGER                               :: LunIn
  INTEGER                               :: LunOut
  INTEGER                               :: iobsub, ihdrdat, pqm, qm, itype
  INTEGER                               :: j, k, i, k1, k2, kl, m
  !---variables for BUFR library
  INTEGER                               :: ireadsub, ireadsb, ireadmg, idate
  !---output SceneAMV format type
  TYPE(SceneAMV_type)                   :: SceneAMV    
  REAL                                  :: ppb, obserr, ediff, del, qify, qifn, ee, ree
  INTEGER(2)                            :: qcflg
  INTEGER                               :: ietabl, lcount, itypex, iflag, nfile, ifile
  INTEGER, PARAMETER                    :: len=256
  CHARACTER(LEN=10)                     :: OutFilePrefix
  CHARACTER(LEN=10)                     :: OutFileSuffix
  !---Pointers and other arrays
  REAL,        DIMENSION(:,:,:),          POINTER     :: etabl
  CHARACTER(LEN=len), DIMENSION(:),       POINTER     :: BUFRFiles,BinOutFiles
  CHARACTER(LEN=1)   :: node 
  !---Namelist data 
  CHARACTER(LEN=len) :: BUFRFilesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: ErrorTableFile=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: pathBinOut=DEFAULT_VALUE_STR4

  !---BUFR variable names to parse
  DATA hdrtr /'SAID CLAT CLON YEAR MNTH DAYS HOUR MINU SWCM SAZA GCLONG SCCF SWQM'/ 
  DATA obstr/'HAMD PRLC WDIR WSPD'/ 
  DATA heightr/'MDPT '/ 
  DATA derdwtr/'TWIND'/
!  DATA satqctr/'RFFL EEQF QIFN QIFY'/
  DATA satqctr/'RFFL '/
  !---originating/generating center, generating application, percent confidence 
  DATA qcstr /'OGCE GNAP PCCF'/
  
  NAMELIST /ControlBUFRdump/BUFRFilesList, ErrorTableFile, PathBinOut 

  !---initialize some variables and defaults
  LunIn     = 10  ! unit for BUFR file 
  LunOut    = 11  ! unit for output SceneAMV_type file
  nqc       = 6   ! default number of QC parameters
  nProfiles = 0
  ErrorTableFile = 'prepobs_errtable.global'  ! from GSI/fix/ this is old file 

  print *, 'ErrorTableFile = ', ErrorTableFile
  print *, '*****************************************************************'
  print *, 'Note: on 14/05/07, GSI trunk error table has inflated observation'
  print *, '      errors for GOES hourly winds (itype = 245, 246).           '
  print *, '      in new ErrorTableFile errors are old errors*2.0            '
  print *, '      ErrorTableFile in NESDIS-JCSDA on EMC SVN also has inflated'
  print *, '      observation errors for GOES hourly winds                   '
  print *, '*****************************************************************'
  
  OutFilePrefix  = 'AMV'
  OutFileSuffix  = '.bin'
  !-----------------------------------------------------
  !     Read control-data from namelist
  !-----------------------------------------------------
  READ(*,NML=ControlBUFRdump)

  !---Read the file names of BUFR data and build output Binary files names
  CALL ReadList4(LunIn,trim(BUFRFilesList),BUFRFiles,nfile,BinOutFiles,pathBinout,&
       trim(OutFilePrefix),trim(OutFileSuffix))
  
  IF (nfile .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(BUFR files in convertBUFR)')

  !---read observation error table

  ALLOCATE(etabl(300,33,6))
  etabl  = 1.e9
  ietabl = 19
  OPEN(ietabl,file=errortablefile,form='formatted')
  REWIND ietabl
  etabl  = 1.e9
  lcount = 0
  loopd : DO
     READ(ietabl,100,IOSTAT=iflag) itypex
     IF( iflag /= 0 ) EXIT loopd  !EOF
     lcount=lcount+1
     DO k=1,33
        READ(ietabl,110)(etabl(itypex,k,m),m=1,6)
     END DO
  END DO   loopd

100 FORMAT(1x,i3)
110 FORMAT(1x,6e12.5)

  IF( lcount <=0 ) THEN
     WRITE(6,*)'ConvertBUFRWind:obs error table not available to 3dvar. the program will stop'
     STOP 
  ELSE
     WRITE(6,*)'ConvertBUFRWind:  observation errors provided by local file errtable'
  ENDIF

  CLOSE(ietabl)

  !-----------------------------------------------------
  !     Loop over the Wind files
  !-----------------------------------------------------
  FilesLoop: DO ifile=1,nfile
     nProfiles = 0
     infile = BUFRFiles(ifile)
     outfile = BinOutFiles(ifile)
     
     !---open the BUFR file and count the number of records
     CALL CLOSBF(lunin)
     OPEN(lunin,file=infile,form='unformatted')
     CALL OPENBF(lunin,'IN',lunin)
     CALL DATELEN(10)
     
     msg_REPORT1: DO WHILE (IREADMG(lunin,subset,idate) == 0)
        loop_REPORT1: DO WHILE (IREADSB(lunin) == 0)
           nProfiles = nProfiles + 1
        ENDDO loop_REPORT1
     ENDDO msg_REPORT1
     CALL CLOSBF(lunin)
     
     !---re-open the file and readin and writeout the records 
     OPEN(lunin,file=infile,form='unformatted')
     CALL OPENBF(lunin,'IN',lunin)
     CALL DATELEN(10)
     
     print *, 'nProfiles = ', nProfiles
     !---Initialize output SceneAMV structure and write header
     CALL InitHdrSceneAMV(nqc,0,0,SceneAMV)
     CALL WriteHdrSceneAMV(LunOut,outfile,SceneAMV,nProfiles)     
     
     nProfiles = 0
     msg_REPORT: DO WHILE (IREADMG(lunin,subset,idate) == 0)
        loop_REPORT: DO WHILE (IREADSB(lunin) == 0)
           qm = 0 
           pqm = 0
           ee  =110.
           qifn=110.
           qify=110.

           nProfiles = nProfiles + 1
           !---Extract type information 
           CALL UFBINT(lunin,hdrdat,13,1,iret,hdrtr)
           !---Extract observation data 
           CALL UFBINT(lunin,obsdat,4,1,iret,obstr)
           qcflg               = 0
           
           !---pre quality check in GSI (read_satwind.f90)
           ppb=obsdat(2) !---Reported wind pressure  
           IF (ppb > 100000000.0 .OR. hdrdat(3) > 100000000.0 &
                .OR. obsdat(4) > 100000000.0) qcflg=1
           IF (ppb > 10000.0) ppb=ppb/100.0
           IF (ppb < 125.0) qcflg = 1                 !  reject data above 125mb

           !---reject the data with bad quality mark from SDM
           IF(INT(hdrdat(13)) == 12 .OR. INT(hdrdat(13)) == 14) qcflg=1
           
           !---determine the satellite wind type as in prepbufr
           !   241: India, 242:JMA visible,243: EUMETSAT visible
           !   245: GOES IR. 246: GOES WV cloud top, 247: GOES WV deep layer
           !   250: JMA WV deep layer. 251:GOES visible, 252: JMA IR
           !   253: EUMETSAT IR , 254: EUMETSAT WV deep layer
           !   257: MODIS IR, 258: WV cloud top, 259:  WV deep layer
           !   DATA qcstr /' OGCE GNAP PCCF'/
           iobsub=INT(hdrdat(1))
           IF(TRIM(subset) == 'NC005064' .OR. TRIM(subset) == 'NC005065' .OR. &
                TRIM(subset) == 'NC005066') THEN ! EUMETSAT
              
              IF( INT(hdrdat(1)) < 70 .and. INT(hdrdat(1)) >= 50) THEN          
                 !---reject data zenith angle > 68.0 degree 
                 ! IF(hdrdat(10) >68.0_r_kind) cycle loop_readsb
                 IF (iobsub == 54) iobsub=0
                 ihdrdat = INT(hdrdat(9))
                 IF(ihdrdat == 1)  THEN                ! IR winds
                    itype=253
                    SceneAMV%DescAlg = 'EUMETSAT IR winds'
                 ELSE IF(ihdrdat == 2) THEN            ! visible winds
                    itype=243
                    SceneAMV%DescAlg = 'EUMETSAT VIS winds'
                 ELSE IF(ihdrdat == 3) THEN            ! WV cloud top, try to assimilate
                    itype=254                                
                    SceneAMV%DescAlg = 'EUMETSAT WV cloud top'
                 ELSE IF(ihdrdat >= 4) THEN            ! WV deep layer,monitoring
                    itype=254
                    SceneAMV%DescAlg = 'EUMETSAT WV deep layer'
                    pqm=9
                    qm=9
                 ENDIF
                 !---Extract quality information
                 CALL UFBREP(lunin,qcdat,3,12,iret,qcstr) 
                 DO j=4,9
                    IF( qify < 105. .and. qifn < 105. .and. ee < 105.) EXIT
                    IF(qcdat(2,j) < 10000. .and. qcdat(3,j) < 10000.) THEN
                       IF(INT(qcdat(2,j)) == 1 .and. qify > 105.) THEN
                          qify=qcdat(3,j)
                       ELSE IF(INT(qcdat(2,j)) == 2 .and. qifn > 105.) THEN
                          qifn=qcdat(3,j)
                       ELSE IF(INT(qcdat(2,j)) == 3 .and. ee > 105.) THEN
                          ee=qcdat(3,j)
                       ENDIF
                    ENDIF
                 ENDDO
                 IF(qifn < 85.0)  THEN
                    qm=15
                    pqm=15
                 ENDIF 

              ENDIF
              
           ELSE IF(TRIM(subset) == 'NC005044' .OR. TRIM(subset) == 'NC005045' .OR. &
                TRIM(subset) == 'NC005046') THEN ! JMA
              
              IF(INT(hdrdat(1)) >=100 .and. INT(hdrdat(1)) <= 199 ) THEN 
                 !IF(hdrdat(10) >68.0_r_kind) cycle loop_readsb   !   reject data zenith angle >68.0 degree 
                 ihdrdat = INT(hdrdat(9))
                 IF(ihdrdat == 1)  THEN                      ! IR winds
                    itype=252
                    SceneAMV%DescAlg = 'JMA IR winds'
                 ELSE IF(ihdrdat == 2) THEN                  ! visible winds
                    itype=242
                    SceneAMV%DescAlg = 'JMA VIS winds'
                 ELSE IF(ihdrdat == 3) THEN                ! WV cloud top 
                    itype=250
                    SceneAMV%DescAlg = 'JMA WV cloud top winds'
                 ELSE IF(ihdrdat >=4) THEN                  ! WV deep layer,as monitoring
                    itype=250
                    SceneAMV%DescAlg = 'JMA WV deep layer winds'
                    qm=9
                    pqm=9
                 ENDIF
                 !---Extract quality information
                 CALL UFBREP(lunin,qcdat,3,12,iret,qcstr) 
                 DO j=4,9
                    IF( qify < 105. .and. qifn < 105. .and. ee < 105.) EXIT
                    IF(qcdat(2,j) < 10000. .and. qcdat(3,j) < 10000.) THEN
                       IF(INT(qcdat(2,j)) == 101 .and. qify > 105.) THEN
                          qify=qcdat(3,j)
                       ELSE IF(INT(qcdat(2,j)) == 102 .and. qifn > 105.) THEN
                          qifn=qcdat(3,j)
                       ELSE IF(INT(qcdat(2,j)) == 103 .and. ee > 105.) THEN
                          ee=qcdat(3,j)
                       ENDIF
                    ENDIF
                 ENDDO
                 IF(qifn < 85.0)  THEN
                    qm=15
                    pqm=15
                 ENDIF 
                 
              ENDIF
              
           ELSE IF(TRIM(subset) == 'NC005010' .OR. TRIM(subset) == 'NC005011' .OR. &
                TRIM(subset) == 'NC005012') THEN ! NESDIS GOES
              
              IF(INT(hdrdat(1)) >=250 .and. INT(hdrdat(1)) <=299 ) THEN
                 
                 !IF(hdrdat(10) >68.0_r_kind) cycle loop_readsb   !   reject data zenith angle >68.0 degree 
                 IF(INT(hdrdat(1)) == 259) iobsub=15 
                 ihdrdat = INT(hdrdat(9))
                 IF(ihdrdat == 1)  THEN                         ! IR winds
                    IF(hdrdat(12) < 50000000000000.0) THEN      ! for channel 4
                       SceneAMV%DescAlg = 'NESDIS GOES IR Ch 4 winds'
                       itype=245
                    ELSE                                        ! for short wave IR
                       SceneAMV%DescAlg = 'NESDIS GOES SW IR winds'
                       itype=240  !esm 14/05/14
                       iobsub=1
                    ENDIF
                 ELSE IF(ihdrdat == 2 ) THEN                   ! visible winds
                    SceneAMV%DescAlg = 'NESDIS GOES VIS winds'
                    itype=251 
                 ELSE IF(ihdrdat == 3) THEN                     ! WV cloud top
                    SceneAMV%DescAlg = 'NESDIS GOES WV cloud top winds'
                    itype=246
                 ELSE IF(ihdrdat >= 4) THEN                     ! WV deep layer.discard
                    SceneAMV%DescAlg = 'NESDIS GOES WV deep layer winds'
                    itype=247
                 ENDIF
                 
                 !---Extract quality information
                 CALL UFBREP(lunin,qcdat,3,8,iret,qcstr)
                 DO j= 1,8
                    IF( qify < 105. .and. qifn < 105. .and. ee < 105.) EXIT
                    IF(qcdat(2,j) <= 10000. .and. qcdat(3,j) < 10000.) THEN
                       IF(INT(qcdat(2,j)) == 1 .and. qify > 105.) THEN
                          qify=qcdat(3,j)
                       ELSE IF(INT(qcdat(2,j)) == 3 .and. qifn > 105.) THEN
                          qifn=qcdat(3,j)
                       ELSE IF(INT(qcdat(2,j)) == 4 .and. ee > 105.) THEN
                          ee=qcdat(3,j)
                       ENDIF
                    ENDIF
                 ENDDO
!                 IF (qifn < 90. .and. INT(qifn) /= 110) THEN  ! esm 14/05/14
                 IF (qifn < 85.) THEN  ! esm 14/05/14 fixed from trunk
                    qm = 15
                    pqm = 15
                 ENDIF
              ENDIF
              
           ELSE IF(TRIM(subset) == 'NC005070' .OR. TRIM(subset) == 'NC005071'  ) THEN ! MODIS
              
              IF(INT(hdrdat(1)) >= 700 .and. INT(hdrdat(1)) <= 799 ) THEN
                 ihdrdat = INT(hdrdat(9))
                 IF(ihdrdat == 1)  THEN                          ! IR winds
                    SceneAMV%DescAlg = 'MODIS IR winds'
                    itype=257
                 ELSE if(ihdrdat == 3) THEN                      ! WV cloud top
                    SceneAMV%DescAlg = 'MODIS WV cloud top winds'
                    itype=258
                 ELSE if(ihdrdat >= 4) THEN                      ! WV deep layer
                    SceneAMV%DescAlg = 'MODIS WV deep layer winds'
                    itype=259 
                 ENDIF
                 !---Extract quality information 
                 CALL UFBREP(lunin,qcdat,3,8,iret,qcstr) 
                 DO j= 1,8
                    IF( qify < 105. .and. qifn < 105. .and. ee < 105.) EXIT
                    IF(qcdat(2,j) <= 10000. .and. qcdat(3,j) < 10000.) THEN
                       IF(INT(qcdat(2,j)) == 1 .and. qify > 105.) THEN
                          qify=qcdat(3,j)
                       ELSE IF(INT(qcdat(2,j)) == 3 .and. qifn > 105.) THEN
                          qifn=qcdat(3,j)
                       ELSE IF(INT(qcdat(2,j)) == 4 .and. ee > 105.) THEN
                          ee=qcdat(3,j)
                       ENDIF
                    ENDIF
                 ENDDO
                 print *, 'MODIS (qifn)===', qifn
                 
              ENDIF
              
           ELSE IF( TRIM(subset) == 'NC005080') THEN                   ! AVHRR  
              
              IF(INT(hdrdat(1)) < 10.0 .OR. (INT(hdrdat(1)) >= 200 .and. &
                   INT(hdrdat(1)) <= 223.0) ) THEN      
                 ihdrdat = INT(hdrdat(9))
                 IF(ihdrdat == 1)  then                            ! IR winds
                    itype=244
                    SceneAMV%DescAlg = 'AVHRR IR winds'
!esm 15/02/26        qm=15  do not automajically reject these winds
!                    pqm=15
                 ELSE
                    WRITE(6,*) 'ConvertBUFRWind: WRONG DERIVED METHOD VALUE'
                 ENDIF
                 !---Extract quality information
                 CALL UFBREP(lunin,qcdat,3,8,iret,qcstr)
                 DO j= 1,6
                    IF( qify <= 105. .and. qifn < 105. .and. ee < 105.) EXIT
                    IF(qcdat(2,j) <= 10000. .and. qcdat(3,j) < 10000.) THEN
                       IF(INT(qcdat(2,j)) == 1 .and. qify > 105.) THEN
                          qify=qcdat(3,j)
                       ELSE IF(INT(qcdat(2,j)) == 3 .and. qifn > 105.) THEN
                          qifn=qcdat(3,j)
                       ELSE IF(INT(qcdat(2,j)) == 4 .and. ee > 105.) THEN
                          ee=qcdat(3,j)
                       ENDIF
                    ENDIF
                 ENDDO
                 print *, 'AVHRR (qifn)===', qifn

              ENDIF
           ELSE IF( TRIM(subset) == 'NC005019') THEN               ! GOES SW winds   
              
              IF(INT(hdrdat(1)) >= 250 .AND. INT(hdrdat(1)) >= 299 ) THEN      
                 ihdrdat = INT(hdrdat(9))
                 IF(ihdrdat == 1)  then                            ! IR winds
                    itype=240
                    SceneAMV%DescAlg = 'NESDIS SW IR winds'
                    qm=15
                    pqm=15
                 ELSE
                    WRITE(6,*) 'ConvertBUFRWind: WRONG DERIVED METHOD VALUE'
                 ENDIF
                 !---Extract quality information
                 CALL UFBREP(lunin,qcdat,3,8,iret,qcstr)
                 DO j= 1,6
                    IF( qify <= 105. .and. qifn < 105. .and. ee < 105.) EXIT
                    IF(qcdat(2,j) <= 10000. .and. qcdat(3,j) < 10000.) THEN
                       IF(INT(qcdat(2,j)) == 1 .and. qify > 105.) THEN
                          qify=qcdat(3,j)
                       ELSE IF(INT(qcdat(2,j)) == 3 .and. qifn > 105.) THEN
                          qifn=qcdat(3,j)
                       ELSE IF(INT(qcdat(2,j)) == 4 .and. ee > 105.) THEN
                          ee=qcdat(3,j)
                       ENDIF
                    ENDIF
                 ENDDO
!                IF (qifn < 90.) THEN 
!                   qm = 15
!                   pqm = 15
!                 ENDIF
              ENDIF
              
           ENDIF
           IF ( qify == 0.) qify=110.
           IF ( qifn == 0.) qifn=110.
           IF (  ee  == 0.) ee=110.

           ppb=MAX(0.0,MIN(ppb,2000.0))
           IF(ppb>=etabl(itype,1,1)) k1=1          
           DO kl=1,32
              IF(ppb>=etabl(itype,kl+1,1).AND.ppb<=etabl(itype,kl,1)) k1=kl
           END DO
           
           IF(ppb<=etabl(itype,33,1)) k1=33    ! esm 14/05/15  was k1 = 5
           k2=k1+1
           
           ediff = etabl(itype,k2,1)-etabl(itype,k1,1)
           IF (ABS(ediff) > TINY(0.0)) THEN
              del = (ppb-etabl(itype,k1,1))/ediff
           ELSE
              del = HUGE(0.0)
           ENDIF
           
           del=MAX(0.0,MIN(del,1.0))
           obserr=(1.0-del)*etabl(itype,k1,4)+del*etabl(itype,k2,4)
           obserr=MAX(obserr,1.0)
           
           !--- GOES observation errors handled slightly differently in trunk 
           !--- esm 14/05/14
           IF (itype.eq.245.or.itype.eq.246) THEN 
             obserr = obserr*2.
             ! Santek QC method
             IF (ee < 105.) THEN 
               ree = (ee - 100)/(-10.0)
               if (obsdat(4) > 0.) THEN 
                  ree = ree / obsdat(4)
               ELSE
                  ree = 2.0
               ENDIF
             ELSE
                ree = 0.2
             ENDIF
             IF (ppb .ge. 800. .and. ree > 0.550) THEN
                qm = 15
                pqm = 15
             ELSEIF (ree > 0.8) THEN
                pqm = 15
                qm = 15
             ENDIF
           ENDIF
                
           !   DATA hdrtr /'SAID CLAT CLON YEAR MNTH DAYS HOUR MINU SWCM SAZA GCLONG SCCF SWQM'/ 
           !---Add basic information to the Scene AMV structure
           SceneAMV%lat        = hdrdat(2)
           SceneAMV%lon        = hdrdat(3)
           IF (ABS(hdrdat(2)) > 62.) print *, 'hi-lat: ', NINT(hdrdat(2))
           SceneAMV%angle      = hdrdat(10)
           SceneAMV%scanYear   = hdrdat(4)
           SceneAMV%scanMonth  = hdrdat(5)
           SceneAMV%scanDay    = hdrdat(6)
           SceneAMV%scanUTC    = hdrdat(7)*3600.0 + hdrdat(8)*60  ! no seconds for now
           SceneAMV%ProfIndx   = nProfiles
           SceneAMV%AlgSN      = 1.0
           SceneAMV%iTyp       = 1          ! satellite retrieval 
           SceneAMV%WindCalcMethod = INT(hdrdat(9))
           
           !   DATA obstr/'HAMD PRLC WDIR WSPD'/ 
           !---Add observation data to the Scene AMV structure
           SceneAMV%HeightMethod = -1
           IF (INT(obsdat(1)) > 0) SceneAMV%HeightMethod  = INT(obsdat(1))
           SceneAMV%WindSp     = obsdat(4)
           SceneAMV%WindU      = -1.*obsdat(4)*SIN(PI/180.*obsdat(3))
           SceneAMV%WindV      = -1.*obsdat(4)*COS(PI/180.*obsdat(3))
           SceneAMV%WindDir    = obsdat(3)  ! Meteorological direction (from which wind is blowing)
           IF (obserr >= 100) qcflg = 1
           SceneAMV%QC(1)      = 0
           SceneAMV%QC(2)      = itype
           SceneAMV%QC(3)      = qcflg
           SceneAMV%QC(4)      = INT(hdrdat(1))
           
           SceneAMV%WindError  = obserr
           ppb = obsdat(2)
           IF (ppb > 10000.0) ppb=ppb/100.0
           SceneAMV%WindPress  = ppb
           !---Add Quality Information to SceneAMV type (from read_satwnd.f90)
           SceneAMV%QI(1)      = ee   ! PERCENT CONFIDENCE BASED ON NESDIS EXPECTED ERROR
           SceneAMV%QI(2)      = qify ! PERCENT CONFIDENCE BASED ON EUMETSAT QUAL INDX W/O FCST
           SceneAMV%QI(3)      = qifn ! PERCENT CONFIDENCE BASED ON EUMETSAT QUAL INDX W/ FCST
           SceneAMV%QI(4)      = qm
!           SceneAMV%QI(5)      = obsdat(5)  ! RFFL 
           
           !---Write SceneAMV to file 
           CALL WriteSceneAMV(LunOut,SceneAMV)
           
        ENDDO loop_REPORT
     ENDDO msg_REPORT

     WRITE(*,*)
     WRITE(*,'(1x,A)')'Input  BUFR File='//TRIM( BUFRFiles(ifile) )
     WRITE(*,'(1x,A)')'Output Binary Wind File='//TRIM( BinOutFiles(ifile) )
     PRINT *, 'Number of profiles processed (total):',nProfiles
     WRITE(*,*)
     !--- Release memory allocated in InitHdrSceneAMV of src/lib/io/IO_SceneAMV.f90
     CALL DestroySceneAMV(SceneAMV)
     !--- Close input and output file units
     CALL CLOSBF(LunIn)
     CLOSE(LunOut)

  ENDDO FilesLoop

  !--- Release memory for filenames and error table data
  DEALLOCATE(BUFRFiles,BinOutFiles)
  DEALLOCATE(etabl)
  
 END PROGRAM AMVBUFR_to_AMVScene
    
           !        testdir = 180./PI * ATAN2(-1.*SceneAMV%WindU,-1.*SceneAMV%WindV)
           !        IF (testdir.lt.0) testdir = testdir + 360.0
