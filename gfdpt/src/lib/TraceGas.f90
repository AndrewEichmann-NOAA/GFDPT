MODULE TraceGas
!------------------------------------------------------------------------
! Name:         TraceGas
! 
! Type:         F90 module
!
! Description:
!       This module is dedicated to the trace gases for MIIDAPS/MIRS and COAT
!
! Modules needed:
!       - Consts
!       - ErrorHandling
!       - IO_Scene
!
! Subroutines contained:
!       - co2firstguess
!       - ch4firstguess
!       - n2ofirstguess
!       - mopittfg
!       - fill_trace
!       - tracetime 
! 
! History:
!       10-17-2014      Eric S. Maddy RTi, Inc. @ NESDIS/STAR/JCSDA  
!                       Created 
!
!------------------------------------------------------------------------
  USE Misc
  USE Consts
  USE IO_Scene
  USE ErrorHandling
  
  IMPLICIT NONE

  PRIVATE  ! make everything in module private by default
  !---Publicly available subroutines
  PUBLIC :: FILL_TRACE, N2OFIRSTGUESS, CO2FIRSTGUESS, MOPITTFG, CH4FIRSTGUESS
  PUBLIC :: TRACETIME  ! could be private 
  !---Publicly available data/type definitions

  !---CO_DEFAULTTYPE = AFGL_MOPITT profile = AIRS v5 FG
  INTEGER, PUBLIC, PARAMETER :: CO_DEFAULTTYPE=2

  !---CO2_DEFAULTTYPE = NOAA/ESRL GLOBALVIEW 2008 fit = AIRS v6 FG
  INTEGER, PUBLIC, PARAMETER :: CO2_DEFAULTTYPE=1

  !---CH4_DEFAULTTYPE = AFGL RTA profile 
  INTEGER, PUBLIC, PARAMETER :: CH4_DEFAULTTYPE=0

  !---CH4_DEFAULTTYPE = Scaled AFGL RTA profile + linear ramp from WDCGG
  INTEGER, PUBLIC, PARAMETER :: N2O_DEFAULTTYPE=1

!  PUBLIC :: COClim_type
!  !---Declaration sections
!  TYPE   :: COClim_type
!     !---climatologies (nmonth, nlon, nlat, npres)
!     real, DIMENSION(:,:,:,:), ALLOCATABLE :: COclim
!     integer  :: nlat_co, nlon_co, nmonth_co
!  END TYPE COClim_type

!  PUBLIC :: CO2Clim_type
!  !---Declaration sections
!  TYPE   :: CO2Clim_type
!     !---climatologies (nmonth, nlon, nlat, npres)
!     real, DIMENSION(:,:,:,:), ALLOCATABLE :: CO2clim
!     integer  :: nlat_co2, nlon_co2, nmonth_co2
!  END TYPE CO2Clim_type

!  PUBLIC :: CH4Clim_type
!  !---Declaration sections
!  TYPE   :: CH4Clim_type
!     !---climatologies (nmonth, nlon, nlat, npres)
!     real, DIMENSION(:,:,:,:), ALLOCATABLE :: CH4clim
!     integer  :: nlat_ch4, nlon_ch4, nmonth_ch4
!  END TYPE CH4Clim_type

!  PUBLIC :: N2OClim_type
!  !---Declaration sections
!  TYPE   :: N2OClim_type
!     !---climatologies (nmonth, nlon, nlat, npres)
!     real, DIMENSION(:,:,:,:), ALLOCATABLE :: N2Oclim
!     integer  :: nlat_n2o, nlon_n2o, nmonth_n2o
!  END TYPE N2OClim_type

!  PUBLIC :: O3Clim_type
!  !---Declaration sections
!  TYPE   :: O3Clim_type
!     !---climatologies (nmonth, nlon, nlat, npres)
!     real, DIMENSION(:,:,:,:), ALLOCATABLE :: O3clim
!     integer  :: nlat_o3, nlon_o3, nmonth_o3
!  END TYPE O3Clim_type

!  PUBLIC :: TraceClim_type
!  !---Declaration sections
!  TYPE   :: TraceClim_type
!     !---climatologies (nmonth, nlon, nlat, npres)
!     real, DIMENSION(:,:,:,:), ALLOCATABLE :: COclim
!     real, DIMENSION(:,:,:,:), ALLOCATABLE :: CO2clim
!     real, DIMENSION(:,:,:,:), ALLOCATABLE :: O3clim
!     real, DIMENSION(:,:,:,:), ALLOCATABLE :: CH4clim
!     real, DIMENSION(:,:,:,:), ALLOCATABLE :: N2Oclim
!     integer                         :: nlat, nlon, nmonth
!  END TYPE TraceClim_type

  !---INTRINSIC functions used in this module
  INTRINSIC :: SUM,ADJUSTL,SIZE,TRIM,ALOG,MAXVAL,EXP,RANDOM_NUMBER,MINVAL
 
CONTAINS

  SUBROUTINE FILL_TRACE(Scene, itypeCO, itypeCO2, itypeN2O, itypeCH4)
    
!=============================================================================
! Name:      FILL_TRACE
!
! Type:      F90 Subroutine
!
! Description:  Initializes Scene trace gas values to user defined 
!               types (itypeCO, itypeCO2, ...)
! Arguments:
!
!      Name        Type (I/O)   Description
!      ----------------------------------------------------------------------
!       - Scene         I/O       Scene structure containing profile and surface data
!       - itypeCO       I         type of first guess for CO
!       - itypeCO2      I         type of first guess for CO2
!       - itypeCH4      I         type of first guess for CH4
!       - itypeN2O      I         type of first guess for N2O
!
! Modules needed:
!  IO_Scene
!  Consts
!
! History:
!       10-17-2014    Eric S. Maddy, created
!
!==============================================================================
    !---IO Scene type -- input/modified on output 
    TYPE(Scene_type)             :: Scene
    INTEGER :: itypeCO, itypeCO2, itypeN2O, itypeCH4
    INTEGER :: iret 
    !---local variables for time and location 
    INTEGER :: kyear, kmonth, kday, khour, kminute
    REAL    :: rsec, alat, alon
    !---local copies of the 100 layer trace gas profiles
    REAL    :: ch4mr(100), pobs(100)
    REAL    :: comr(100), n2omr(100), co2mr(100)
    !---Indexes
    INTEGER, DIMENSION(:), ALLOCATABLE          :: idx_LayEff,idx_LevEff
    INTEGER                                     :: i,iLay,nLayEff,nLevEff

    iret = 0

    IF (Scene%nAbsorb > 2) THEN
       !---Adjust scene p,t,w layers,nLay,nLev for valid layers only
       nLayEff  = COUNT(Scene%Temp_lay .gt. 0. .and. &
            Scene%Pres_lev(1:Scene%nLay) .le. Scene%SfcPress)
       IF (nLayEff <= 2) THEN
          Scene%qc(1) = 1
          RETURN
       ELSE
          nLevEff  = nLayEff + 1
          ALLOCATE(idx_LayEff(nLayEff),idx_LevEff(nLevEff))
          idx_LayEff = PACK( (/(i,i=1,SIZE(Scene%Temp_lay))/), & 
               (Scene%Temp_lay .gt. 0. .and. Scene%Pres_lev(1:Scene%nLay) .le. Scene%SfcPress))
          idx_LevEff(1:nLayEff) = idx_LayEff(1:nLayEff)
          idx_LevEff(nLevEff)   = idx_LayEff(nLayEff) + 1  
          DO iLay=1,Scene%nLay-1
            IF (Scene%Absorb_lay(iLay,Scene%ih2o) .lt. 0) &
                 Scene%Absorb_lay(iLay,Scene%ih2o) = 0.
          ENDDO
       ENDIF
       !---copy time and location information to 
       !   the structure
       kyear  = Scene%scanYear
       CALL DAY_MONTH(Scene%scanYear,kmonth,kday,Scene%scanDay)
       khour   = 0
       kminute = 0
       rsec    = Scene%scanUTC   ! seconds within the day 
       alat    = Scene%lat
       alon    = Scene%lon
       !---add CO 
       IF (Scene%nAbsorb >= 3) THEN
          CALL MOPITTFG (itypeCO, alat, &
          alon, kyear, kmonth, kday, khour, kminute, rsec, comr, iret)
          Scene%Absorb_lay((idx_LayEff(1:nLayEff)),Scene%iCO)=comr((idx_LayEff(1:nLayEff)))
       ENDIF   
       !---add CO2
       IF (Scene%nAbsorb >= 4) THEN
          CALL CO2FIRSTGUESS (itypeCO2, alat, &
           alon, kyear, kmonth, kday, khour, kminute, rsec, &
           co2mr, iret)
          Scene%Absorb_lay((idx_LayEff(1:nLayEff)),Scene%iCO2)=co2mr((idx_LayEff(1:nLayEff)))
       ENDIF   
       !---add CH4
       IF (Scene%nAbsorb >= 5) THEN
          pobs(1:nlayeff) = scene%pres_lay(1:nlayeff) 
          CALL CH4FIRSTGUESS (itypeCH4, alat, pobs, ch4mr, iret)
          Scene%Absorb_lay((idx_LayEff(1:nLayEff)),Scene%iCH4)=ch4mr((idx_LayEff(1:nLayEff)))
       ENDIF   
       !---add N2O
       IF (Scene%nAbsorb >= 6) THEN
          CALL N2OFIRSTGUESS (itypeN2O, kyear, kmonth, &
               kday, khour, kminute, rsec, &
               alat, n2omr, iret)
          Scene%Absorb_lay((idx_LayEff(1:nLayEff)),Scene%iN2O)=n2omr((idx_LayEff(1:nLayEff)))
       ENDIF
       !---cleanup allocatable arrays
       DEALLOCATE(idx_LayEff,idx_LevEff)
    ENDIF
  END SUBROUTINE FILL_TRACE

  SUBROUTINE N2OFIRSTGUESS (itype, &
       kyear, kmonth, kday, khour, kminute, rsec, &
       alat, n2omr, iret)
    
    !     PASSED VARIABLES
    !     ---------------
    INTEGER    ::    itype ! (0=UMBC REF, 1=LINEAR RAMP)
    INTEGER    ::    iret  ! return flag
    INTEGER    ::    kyear, kmonth, kday, khour, kminute
    REAL       ::    rsec, alat
    
    !     local variables
    !     ---------------
    INTEGER    ::     i, L
    !     esm: reference for n2oslope
    !     global fit to monthly wdcgg data from 2002 to 2008
    !     fit performed 08/14/2008 
    REAL, PARAMETER ::  n2oslope = 0.741205  
    
    REAL  ::      N2Omr(100)
    REAL  ::      AFGLmr(100)    ! AFGL fg in ppb (f/ Larrabee model)
    REAL  ::      ACTMmr(100)    ! AFGL fg in ppb (f/ Larrabee model)
    REAL  ::      rdate
    REAL, PARAMETER ::  rzero = 0.0
    
    !     this data is from the UMBC v9 RTA using idl/trace/rd_rtan2o.pro
    !     N2O in ppbv
    DATA AFGLmr/ 0.7235, 0.9505, 1.2216, 1.5390, 1.9404, 2.4686,&
         3.1095,   4.0569,   5.8402,  10.4406,  16.5736,  25.3028,&
         36.5502,  48.6552,  60.8776,  74.0556,  88.6407, 101.6174,&
         113.4274, 123.7926, 133.1518, 141.8944, 148.5608, 154.5503,&
         160.2755, 165.6368, 170.7722, 175.8489, 183.2206, 190.7350,&
         196.7889, 202.2362, 208.2127, 216.4880, 225.4497, 234.9410,&
         243.8336, 252.2760, 259.8441, 266.9113, 272.7134, 277.9518,&
         282.5450, 286.7393, 290.3115, 293.1487, 295.8275, 298.0826,&
         300.2454, 302.2226, 304.0618, 305.8470, 307.5672, 309.2195,&
         310.8214, 312.3524, 313.8181, 315.2609, 316.5132, 317.7593,&
         318.8231, 319.3687, 319.8517, 320.3039, 320.4974, 320.6488,&
         320.7748, 320.8148, 320.8095, 320.8055, 320.7975, 320.8053,&
         320.7993, 320.8043, 320.8249, 320.8320, 320.8532, 320.8714,&
         320.9035, 320.9450, 320.9850, 321.0334, 321.0983, 321.1590,&
         321.2219, 321.2975, 321.3983, 321.4897, 321.5784, 321.6732,&
         321.7603, 321.8539, 321.9339, 322.0431, 322.1482, 322.2521,&
         322.3520, 322.4543, 322.5554, 322.6559/
    
    
    !c ---- new version of firstguess, by Xiaozhen Xiong, 1/15/2013
    !c actmmr is from model ACTM in tropics, but the top 12 is averaged [50s,50N]
    DATA actmmr/1.4368,  1.4745,  1.6613, 2.2578, 3.3328, 4.8269, &
         6.3981,  7.8499,  9.3864, 11.4764, 14.2196, 18.0874, &
         33.0370, 43.8854, 57.6226, 74.4356, 93.3894, 114.6412,&
         136.9425, 159.8920, 182.4054, 202.9855, 221.3385, 237.1330,&
         249.7450, 260.4395, 269.7802, 277.5649, 284.2837, 290.3261,&
         295.4279, 299.8174, 303.9820, 307.7712, 311.0118, 313.8315,&
         316.2280, 318.0359, 319.4496, 320.6711, 321.5672, 322.2928,&
         322.8076, 323.1569, 323.3771, 323.5208, 323.6221, 323.6970,&
         323.7460, 323.7805, 323.8024, 323.8171, 323.8247, 323.8291,&
         323.8309, 323.8301, 323.8275, 323.8228, 323.8174, 323.8107,&
         323.8046, 323.7978, 323.7917, 323.7855, 323.7813, 323.7776,&
         323.7744, 323.7711, 323.7683, 323.7686, 323.7698, 323.7734,&
         323.7780, 323.7862, 323.7938, 323.8024, 323.8084, 323.8148,&
         323.8203, 323.8238, 323.8275, 323.8304, 323.8351, 323.8377,&
         323.8410, 323.8446, 323.8494, 323.8446, 323.8402, 323.8385,&
         323.8301, 323.8110, 323.7942, 323.7761, 323.7610, 323.8171,&
         324.1086, 324.1086, 324.1086, 324.1086/ 
    
    
    !      DATA f /0.5, 7.0, 61./
    
    CALL TRACETIME(kyear,kmonth,kday,khour,kminute,rsec,rdate)
    
    if(itype == 0) then
       do L = 1, 100
          N2Omr(L) = AFGLmr(L)*1.e-3
       enddo
    elseif(itype == 1) THEN
       do L = 1, 100
          N2Omr(L) = (AFGLmr(L) + n2oslope*rdate)*1.e-3
       enddo
!    elseif(itype.eq.2) THEN
!       do L = 1, 100
!          N2Omr(L) = ACTMfitmr(L) 
!       enddo
!    elseif(itype.eq.3) THEN
!       do L = 1, 100
!          N2Omr(L) = ACTMfitmr(L) + n2oslope*rdate
!       enddo
    else
       PRINT 1310, itype
       i = 8
       N2Omr(1:100) = AFGLmr(1:100)*1.e-3
    endif
    
    RETURN
815 FORMAT('=======================================================')
816 FORMAT(' N2Ofirstguess: replace fg for N2O w/ UMBC fg')
1300 FORMAT(' N2Ofirstguess error: only works for 100 levels:',i4)
1310 FORMAT(' N2Ofirstguess error: itype DOES not exist:',i4,/,' setting N2O to AFGL')
  END SUBROUTINE N2OFIRSTGUESS
  SUBROUTINE MOPITTFG (itype, alat, &
       alon, kyear, kmonth, kday, khour, kminute, rsec, comr, iret)
    
    !     PASSED VARIABLES
    !     ---------------
    INTEGER   ::    itype ! (0=AFGL, 1=MOPITT, 2=AFGL/MOPITT, 3=MOPITTV4CLIM)
    REAL      ::    alat, alon
    INTEGER   ::    kyear, kmonth, kday, khour, kminute
    REAL      ::    rsec
    INTEGER   ::    iret  ! return flag
    
    !     local variables
    !     ---------------
    INTEGER   ::     i, L
    REAL      ::     COmr(100)
    REAL      ::     AFGLmr(100)    ! AFGL fg in ppb (f/ Larrabee model)
    REAL      ::     MOPITTmr(100)  ! MOPITT fg in volumetric mixing ratio, ppb
    REAL      ::     AFGL_MOPP(100) ! merged AFGL(top) and MOPPITT (bottom)
    !      REAL      ::      dco1
    !      INTEGER   ::     Lsplice
    
    !     AFGL_MOPP is used in v5 PGE because
    !     a) MOPITTmr() caused out of range errors in RTA at the top
    !     b) Scott Hannon said it was unREAListic
    !     c) we have no sensitivity there anyway
    !     d) merged by J. Blaisdell   10/4/06
    !
    !---this data is from the v8 RTA using idl/trace/rd_rtach4.pro
    DATA AFGLmr/1751.373, 652.180, 313.65, 198.207, 132.753,&
         93.20174,  69.14629,  55.71019,  45.23621,  37.38374,&
         32.96809,  29.58296,  26.98655,  24.92291,  23.32885,&
         21.93244,  20.72882,  19.77256,  18.97373,  18.33862,&
         17.80860,  17.30782,  16.88198,  16.48431,  16.10803,&
         15.77875,  15.46739,  15.15361,  14.57103,  13.90767,&
         13.29397,  12.78400,  12.42522,  12.39883,  12.66797,&
         13.22867,  14.18790,  15.37360,  17.23726,  19.33063,&
         21.72738,  24.19936,  26.77901,  29.38781,  32.48757,&
         36.21248,  39.99961,  44.34575,  48.67810,  53.50459,&
         58.65868,  63.73641,  68.95757,  74.12170,  78.96470,&
         82.98607,  86.86616,  90.56042,  93.86670,  97.09173,&
         100.25983, 103.39190, 106.47180, 109.48003, 112.34605,&
         115.13886, 117.87411, 120.04993, 121.90269, 123.72630,&
         125.34476, 126.56119, 127.74594, 128.87616, 129.47939,&
         129.90816, 130.33141, 130.70741, 130.97739, 131.23795,&
         131.49626, 132.16161, 133.17923, 134.17603, 135.17546,&
         136.40681, 137.73354, 139.04582, 140.34218, 141.66153,&
         142.96844, 144.26489, 145.55246, 146.82574, 148.08551,&
         149.32925, 150.56126, 151.77051, 152.97118, 154.15443/
    
    !---this data is from Wallace McMillan and based on MOPITT v3
    !   a priori
    DATA MOPITTmr/ 24.2409,  24.2168,  24.1745,  24.1103,  24.0187,&
         23.8946,  23.7348,  23.5373,  23.3026,  23.0313,&
         22.7265,  22.3909,  22.0266,  21.6284,  21.1948,&
         20.7322,  20.2505,  19.7646,  19.2927,  18.8556,&
         18.4784,  18.1747,  17.9416,  17.7607,  17.5914,&
         17.3647,  17.0681,  16.8321,  16.8665,  17.4449,&
         18.6399,  20.3268,  22.3094,  24.4428,  26.5907,&
         28.6583,  30.6771,  32.7125,  34.8565,  37.1747,&
         39.6367,  42.1871,  44.7521,  47.2370,  49.5845,&
         51.8020,  53.9116,  55.9452,  57.9489,  59.9825,&
         62.0931,  64.2530,  66.4136,  68.5153,  70.4870,&
         72.2682,  73.8662,  75.3098,  76.6364,  77.8896,&
         79.0853,  80.2262,  81.3165,  82.3502,  83.2717,&
         84.0038,  84.4653,  84.6851,  84.8021,  84.9779,&
         85.2669,  85.5724,  85.7798,  85.8779,  85.9660,&
         86.1554,  86.5235,  87.1354,  87.9981,  88.9903,&
         89.9855,  91.0270,  92.2368,  93.5934,  94.9150,&
         96.2908,  98.3620, 101.2763, 103.5686, 104.2298,&
         104.8466, 106.3291, 107.6906, 110.6079, 114.1500,&
         118.1522, 120.4714, 120.6016, 120.6461, 120.6100/
    
    !---this data is a combination of AFGL above 10mb and MOPITT below
    DATA AFGL_MOPP/ 1751.373, 652.180, 313.65, 198.207, 132.753,&
         93.20174, 69.14629, 55.71019, 45.23621, 37.38374,&
         32.96809, 29.58296, 26.98655, 24.92291, 23.32885,&
         21.93244, 20.72882, 19.77256, 19.2927,  18.8556,&
         18.4784,  18.1747,  17.9416,  17.7607,  17.5914,&
         17.3647,  17.0681,  16.8321,  16.8665,  17.4449,&
         18.6399,  20.3268,  22.3094,  24.4428,  26.5907,&
         28.6583,  30.6771,  32.7125,  34.8565,  37.1747,&
         39.6367,  42.1871,  44.7521,  47.2370,  49.5845,&
         51.8020,  53.9116,  55.9452,  57.9489,  59.9825,&
         62.0931,  64.2530,  66.4136,  68.5153,  70.4870,&
         72.2682,  73.8662,  75.3098,  76.6364,  77.8896,&
         79.0853,  80.2262,  81.3165,  82.3502,  83.2717,&
         84.0038,  84.4653,  84.6851,  84.8021,  84.9779,&
         85.2669,  85.5724,  85.7798,  85.8779,  85.9660,&
         86.1554,  86.5235,  87.1354,  87.9981,  88.9903,&
         89.9855,  91.0270,  92.2368,  93.5934,  94.9150,&
         96.2908,  98.3620, 101.2763, 103.5686, 104.2298,&
         104.8466, 106.3291, 107.6906, 110.6079, 114.1500,&
         118.1522, 120.4714, 120.6016, 120.6461, 120.6100/ 
    
    if(itype.eq.0) then
       do L = 1, 100
          COmr(L) = AFGLmr(L)*1.e-3
       enddo
    elseif(itype.eq.1) then
       do L = 1, 100
          COmr(L) = MOPITTmr(L)*1.e-3
       enddo
    elseif(itype.eq.2) then
       do L = 1, 100
          COmr(L) = AFGL_MOPP(L)*1.e-3
       enddo
       !      elseif(itype.eq.3) then
       !         CALL GET_MOPITT_V4CLIM(alat, alon, kyear, 
       !     $        kmonth, kday, khour, kminute, rsec, comr, iret)
       !      elseif(itype.eq.4) then
       !        CALL GET_MOPITT_V4CLIM(alat, alon, kyear, 
       !     $        kmonth, kday, khour, kminute, rsec, comr, iret)
       !        Lsplice = 20
       !        dco1 = (comr(Lsplice)-afgl_mopp(Lsplice))/
       !     &       (pobs(Lsplice)*pobs(Lsplice)*afgl_mopp(Lsplice))
       !        do L = 1, Lsplice-1
       !           comr(L) = afgl_mopp(L)*(1.0 + dco1*pobs(L)*pobs(L))
       !        enddo
    else
       PRINT 1310, itype
       i = 8
    endif
    
    RETURN
815 FORMAT('=======================================================')
816 FORMAT(' MOPITTfg: replace fg for CO w/ fg(',i3,')')
1300 FORMAT(' MOPITTfg error: only works for 100 levels:',i4)
1310 FORMAT(' MOPITTfg error: itype DOES not exist:',i4)
  END SUBROUTINE MOPITTFG
  
  SUBROUTINE CH4FIRSTGUESS (ifit, alat, pobs, ch4mr, iret)
    
    !     PASSED VARIABLES
    !     ---------------
    INTEGER   :: ifit   ! fg type
    !           ifit=0 AFGL profile from RTA
    !           ifit=1 X. Xiong's fit 
    INTEGER   :: iret
    REAL      :: alat
    
    !     local variables
    !     ---------------
    INTEGER   ::  i, L
    REAL      ::  ch4mr(100)   ! fg in volumetric mixing ratio, ppb
    REAL      ::  pobs(100)
    
    !     ---------------- ifit=0 AFGL f/ UMBC RTA -----------
    REAL      ::  ch4mr_afgl(100)  ! fg in volumetric mixing ratio, ppb
    !     ---------------- ifit = 4 and 6 -------------------
    REAL      ::  f(3)          ! coefficients for ifit = 4
    REAL      ::  ff(3)         ! coefficients for ifit = 6
    REAL      ::  coeff(11)     ! coefficients for ifit = 4
    REAL      ::  sum, XX(12)   ! predictors (temporary array)
    
    !     ---------------- ifit=0 AFGL f/ UMBC RTA -----------
    !     This data is from the v8a RTA ch4std() converted to mixing
    !     ratio using idl/trace/rd_rtach4.pro
    DATA ch4mr_afgl/ &     
         162.775, 162.478, 162.230, 162.023, 161.869, 167.896,&
         179.989, 204.920, 240.769, 310.313, 390.160, 471.206,&
         548.196, 617.749, 676.358, 730.520, 780.766, 827.680,&
         871.754, 909.093, 941.959, 973.163, 1001.66, 1028.65,&
         1054.25, 1077.26, 1099.21, 1121.18, 1160.31, 1207.91,&
         1259.31, 1312.11, 1364.61, 1416.57, 1462.90, 1503.24,&
         1537.87, 1568.83, 1592.84, 1613.83, 1631.24, 1647.03,&
         1661.60, 1675.32, 1687.37, 1697.43, 1707.13, 1715.99,&
         1724.54, 1732.49, 1739.94, 1747.16, 1753.67, 1759.95,&
         1765.80, 1770.52, 1775.08, 1779.34, 1782.84, 1786.25,&
         1789.42, 1791.92, 1794.35, 1796.58, 1797.95, 1799.11,&
         1800.26, 1801.02, 1801.57, 1802.04, 1802.61, 1803.21,&
         1803.79, 1804.38, 1804.55, 1804.65, 1804.81, 1804.95,&
         1805.08, 1805.33, 1805.57, 1805.86, 1806.17, 1806.53,&
         1806.90, 1807.28, 1807.87, 1808.41, 1808.89, 1809.35,&
         1809.89, 1810.43, 1810.85, 1811.44, 1812.08, 1812.62,&
         1813.20, 1813.82, 1814.39, 1814.93/


    !     ---------------- ifit=6 --------------------------
    !     improvement in the HNH based on ifit=4  
    !     f/ Xiaozhen Xiong 8/5/2008 (see comments below)
    DATA ff/-0.35, 7.1, 150./
    
    DATA coeff/7.308, 0.0339, 0.0483, -0.1394, -0.1279,0.0624,&
         0.0214, -0.0119, -0.0134, -0.0102,  0.00224 /
    
    
    !     ============================================================
    !     ================ beginning of executable code ===============
    !     ============================================================
    
    !     -------------------------------------------
    !     first, ensure that product is on 100 levels
    !     otherwise this code doesn't work.
    !     -------------------------------------------
    
    !     --------------------------------------------------
    !     create first guess dry mixing ratio based on ifit
    !     --------------------------------------------------
    
    if(ifit.eq.0) then
       do L = 1, 100
          ch4mr(L) = ch4mr_afgl(L)*1.e-3
       enddo
       
    elseif(ifit.eq.1) then
       
       !  Variant of ifit = 6  ! 12/10/10 
       do L = 1,100
          
          XX(1) = sin(alat *pi/180.)
          XX(2) = SQRT(Pobs(L))
          XX(3) = alog(1.+Pobs(L))
          XX(4)= XX(1)**2
          XX(5)= XX(3)**2
          XX(6)= XX(1)**2*XX(3)
          XX(7)= XX(1)*XX(3)
          XX(8)= XX(1)**3
          XX(9)= XX(3)**3
          XX(10)= XX(1)*XX(3)**2
          XX(11)= alog(20.+Pobs(L))* cos(alat*pi/180.)**8
          XX(12)= 1. + XX(1)**8
          
          sum = coeff(1)
          do i = 2,11
             sum = sum + coeff(i)*XX(i-1)
          enddo
          
          ch4mr(L)= 1.e-3*(exp(sum) - exp(ff(1)*XX(11) + &
               + ff(2)/(1.+Pobs(L)/(ff(3)*XX(12)) ))* &
               exp((-alog(100./(50. + Pobs(L)))**8)))
          
       enddo ! loop over L 
       
    else
       
       PRINT 1400, ifit
       i = 8
       
    endif
    
    RETURN
100 FORMAT(i5,3f10.3)
815 FORMAT('=======================================================')
816 FORMAT(' CH4fg: replace fg for CH4 w/ RTA STD fg, lat=',f5.1)
1300 FORMAT(' CH4fg error: only works for 100 levels:',i4)
1400 FORMAT(' CH4fg error: ch4fgtype is invalid:',i4)
    
  END SUBROUTINE CH4FIRSTGUESS
  
  SUBROUTINE CO2FIRSTGUESS(ifit, alat, &
       alon, kyear, kmonth, kday, khour, kminute, rsec, co2mr, iret )
    
    !     input variables
    !     ===============
    INTEGER ifit  ! fit type = namelist PARAMETER CO2fgtype
    REAL    alat, alon
    INTEGER kyear, kmonth, kday
    INTEGER julianday
    INTEGER khour, kminute
    REAL    rsec
    
    !     output variables
    !     ===============
    
    INTEGER iret
    
    !     local variables
    !     ===============
    REAL co2fit 
    REAL co2mr(100)
    REAL time
    INTEGER L
    
    CALL TRACETIME(kyear,kmonth,kday,khour,kminute,rsec,time)
    
    IF (ifit.eq.0) then
       
       co2fit = 385.0
       
    ELSEIF (ifit.eq.1) THEN 
       !     Linear 2 PARAMETER fit to globally averaged CMDL CO2 
       !     between 2002. and 2008.
       !     file:  /home/emaddy/cmdl/gv_co2/gv_2008/ref_mbl_mtx.co2
       !    
       !     NOTES: 
       !     differences between latitude dependent mbl product and 
       !     linear fit is largest poleward 90N (MAX -> 10 ppmv); however 
       !     globally the linear 2 PARAMETER fit compares within +/- 
       !     2.02 ppmv (1-sigma) for marine sites.
       ! 
       !     Coefficients were determined by first selecting the raw 
       !     NOAA/ESRL GLOBALVIEW-CO2 2009 surface CO2 data for marine 
       !     sites between 01/01/2002 and 12/31/2009.  These were then 
       !     smoothed using the routine ccgcrv.c (available from NOAA/ESRL)
       !     and fit to a linear polynomial.  
       !     Currently used in v6 
       !     Eric S. Maddy 
       co2fit = 371.789948 + 2.026214*time 
       
    ELSE
       
       L = 8
       PRINT 1650, ifit
       
    ENDIF
    
    co2mr(1:100) = co2fit
    
    RETURN
800 FORMAT('CO2firstguess: prof=',i8,' lat bin not found, alat=',f5.1)
815 FORMAT('=======================================================')
816 FORMAT('CO2firstguess: replace CO2 with CMDL fit for latitude:',&
         /,'  fit=',i2,' lat=',f5.1,'  mm/dd/yy=',i2,'/',i2,'/',i4)
1650 FORMAT('CO2firstguess: invalid type = ',i4)
    
  END SUBROUTINE CO2FIRSTGUESS
  SUBROUTINE TRACETIME(kyear,kmonth,kday,khour,kminute,rsec,time)
    INTEGER :: kyear,kmonth,kday,khour,kminute
    REAL    :: rsec
    REAL    :: time
    REAL    :: rdays, day
    INTEGER :: i
    REAL    :: days(12)   ! Julian Day number of DY=0 for each month
    
    DATA days/   0.0,  31.0,  59.0,  90.0, 120.0, 151.0, &
         181.0, 212.0, 243.0, 273.0, 304.0, 334.0 /
    
    i = kyear/4
    i = i*4                 
    
    rdays = 365.0
    day  = float(kday) + days(kmonth) 
    if(i.eq.kyear) THEN  ! leap year
       rdays = 366.0           
       IF(kmonth.gt.2) day = day + 1.0
    endif
    
    day = day + (float(khour) + (float(kminute) + rsec/60.)/60.)/24.0
    
    time = float(kyear) + day/rdays - 2002.0 ! close enough
    
  END SUBROUTINE TRACETIME
END MODULE TraceGas
