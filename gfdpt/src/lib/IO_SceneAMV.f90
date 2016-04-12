!$Id: IO_SceneAMV.f90 2782 2014-04-02 12:00:59Z emaddy $
!-----------------------------------------------------------------------------------------------
! Name:         IO_SceneAMV
! 
! Type:         F90 module
!
! Description:
!       This module is dedicated to the I/O of the scene AMV data.
!
! Modules needed:
!       - misc
!       - Consts
!       - utils
!       - ErrorHandling
!
! Subroutines contained:
!       - set2defaultAMV
!       - InitHdrSceneAMV
!       - SetUpSceneAMV
!       - WriteHdrSceneAMV
!       - WriteSceneAMV
!       - ReadHdrSceneAMV
!       - ReadSceneAMV
!       - PrintSceneAMV
!       - DestroySceneAMV
!
! Data type included:
!       - SceneAMV_type
!
! 
! History:
!       04-02-2014      Eric S. Maddy RTi  Created 
!
!-----------------------------------------------------------------------------------------------

MODULE IO_SceneAMV
  USE misc
  USE Consts
  USE utils
  USE ErrorHandling
  IMPLICIT NONE
  PRIVATE
  !---Publicly available subroutine
  PUBLIC :: InitHdrSceneAMV,SetUpSceneAMV,WriteHdrSceneAMV,WriteSceneAMV
  PUBLIC :: ReadHdrSceneAMV,ReadSceneAMV, DestroySceneAMV
  PUBLIC :: set2defaultAMV, PrintSceneAMV
!,FindIndxStr
  !---Publicly available data/type definitions
  PUBLIC :: SceneAMV_type
  !---Declaration sections
  TYPE   :: SceneAMV_type
    INTEGER                                    :: iTyp       ! 0->Scene, 1->retrieved scene (with ChiSq, etc)
    INTEGER                                    :: AlgSN      ! Algorithm Serial number (svn). For algorithm-based scenes)
    INTEGER                                    :: ProfIndx   ! Profile Index 
    !---Algorithm Information
    CHARACTER(LEN=40)                          :: DescAlg    ! Algorithm Description
    INTEGER                                    :: WindCalcMethod ! Wind Calculation Algorithm Method
    INTEGER                                    :: HeightMethod   ! Height Assignment Method
    !---Positioning Data
    REAL                                       :: angle      ! angle
    REAL                                       :: lat        ! Latitude
    REAL                                       :: lon        ! Longitude
    INTEGER                                    :: node       ! =0->ASC, =1->DESC
    INTEGER                                    :: scanDAY    ! Day 
    INTEGER                                    :: scanYear   ! Year
    INTEGER                                    :: scanMonth  ! Month
    REAL                                       :: scanUTC    ! UTC time
    INTEGER                                    :: iscanPos   ! Scan position 
    INTEGER                                    :: iScanLine  ! Scan line Index 
    INTEGER                                    :: nPosScan   ! Number of scan positions within scanline
    INTEGER                                    :: nScanLines ! Number of scanlines within orbit (some might be missing)
    !---Point Wind Parameters
    REAL                                       :: WindSp     ! Wind speed
    REAL                                       :: WindDir    ! Wind vector
    REAL                                       :: WindU      ! U-direction wind speed
    REAL                                       :: WindV      ! V-direction wind speed
    REAL                                       :: WindPress  ! Pressure of AMV
    REAL                                       :: WindError  ! Error Estimate of AMV
    !---QC info 
    INTEGER                                    :: nqc        ! #Elements in QC
    INTEGER(2),        DIMENSION(:),   POINTER :: qc         ! QC vector
    REAL,              DIMENSION(:),   POINTER :: qi         ! quality vector
    !---Dummy placeholder variables
    INTEGER                                    :: dummy      ! dummy variable (placeholder)
  END TYPE SceneAMV_type
  
  !---INTRINSIC functions used in this module
  INTRINSIC :: SUM,ADJUSTL,SIZE,TRIM,ALOG,MAXVAL,EXP,RANDOM_NUMBER,MINVAL
 
CONTAINS

!===============================================================
! Name:         set2defaultAMV
!
!
! Type:         Subroutine
!
!
! Description:  set to default values all SceneAMV 
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - SceneAMV              I/O              Input scene structure
!
! Modules needed:
!       - Consts
!
!
! History:
!       04-02-2014      Eric S. Maddy
!
!===============================================================
  SUBROUTINE set2defaultAMV(SceneAMV)
    TYPE(SceneAMV_type)      :: SceneAMV

    !---Some variables
    SceneAMV%WindSp      = DEFAULT_VALUE_REAL
    SceneAMV%HeightMethod   = DEFAULT_VALUE_INT
    SceneAMV%WindCalcMethod = DEFAULT_VALUE_INT
    SceneAMV%WindDir     = DEFAULT_VALUE_REAL
    SceneAMV%WindU       = DEFAULT_VALUE_REAL
    SceneAMV%WindV       = DEFAULT_VALUE_REAL
    SceneAMV%WindPress   = DEFAULT_VALUE_REAL
    SceneAMV%WindError   = DEFAULT_VALUE_REAL

  END SUBROUTINE set2defaultAMV

!===============================================================
! Name:         InitHdrSceneAMV
!
!
! Type:         Subroutine
!
!
! Description:  Initialize the header of the sceneAMV structure 
!               with general. It also performs the memory allocation for the arrays.
!
!
! Arguments:
!
!           Name                    Type            Description
!      ---------------------------------------------------
!       - nqc                       I             # of integers used for QC
!       - iTyp                      I             Type of file: retrieval of truth
!       - nPosScan                  I             # of scan positions within scanline
!
!
! Modules needed:
!       - None
!
!
! History:
!       04-02-2014      Eric S. Maddy RTi  Created !
!===============================================================

  SUBROUTINE InitHdrSceneAMV(nqc,iTyp,AlgSN,SceneAMV)
    INTEGER                         :: nqc
    TYPE(SceneAMV_type)             :: SceneAMV
    INTEGER                         :: iTyp !=0->Simple SceneAMV, =1->Retrieved SceneAMV 
    INTEGER                         :: AlgSN
    !---Initialize the SceneAMV structure (individual values)
    SceneAMV%iTyp                = iTyp
    SceneAMV%AlgSN               = AlgSN
    SceneAMV%nqc                 = nqc
    ALLOCATE(SceneAMV%qc(nqc))
    ALLOCATE(SceneAMV%qi(nqc))

    RETURN
  END SUBROUTINE InitHdrSceneAMV



!===============================================================
! Name:         ReadHdrSceneAMV
!
!
! Type:         Subroutine
!
!
! Description:  Reads the header from a geophysical SceneAMV file.
!               It also performs memory allocation for arrays 
!               needed later on.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - iu                 O              Unit number after opening
!       - InputFile          I              Name of the input file
!       - SceneAMV              I/O            Structure containing geoph data
!       - nprf               O              Number of profiles in file
!
!
! Modules needed:
!       - None
!
!
! History:
!       04-02-2014      Eric S. Maddy RTi  Created 
!
!===============================================================

  SUBROUTINE ReadHdrSceneAMV(iu,InputFile,SceneAMV,nprf)
    CHARACTER(LEN=*)      :: InputFile
    INTEGER               :: iu,nPrf
    TYPE(SceneAMV_type)      :: SceneAMV
    !---Open file 
    iu=get_lun()
    OPEN(iu,file=InputFile,form='unformatted')
    !---iTyp: =0->Simple SceneAMV, =1->Retrieved SceneAMV 
    READ(iu)  SceneAMV%iTyp,SceneAMV%AlgSN
    READ(iu)  nprf
    READ(iu)  SceneAMV%nqc
    !---Allocate the arrays for future reading
    ALLOCATE(SceneAMV%qc(SceneAMV%nqc))
    ALLOCATE(SceneAMV%qi(SceneAMV%nqc))
    RETURN
  END SUBROUTINE ReadHdrSceneAMV

!===============================================================
! Name:         WriteHdrSceneAMV
!
!
! Type:         Subroutine
!
!
! Description:  Writes header of scene in to file
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - iu                 O             Unit number
!       - OutputFile         I             Name of output file
!       - SceneAMV              I             Structure whose header 
!                                          is to be written out
!       - nprf               I             Number of profiles to write
!
!
! Modules needed:
!       - None
!
!
! History:
!       04-02-2014      Eric S. Maddy RTi  Created 
!
!===============================================================

  SUBROUTINE WriteHdrSceneAMV(iu,OutputFile,SceneAMV,nprf)
    CHARACTER(LEN=*)      :: OutputFile
    INTEGER               :: iu,nPrf
    TYPE(SceneAMV_type)      :: SceneAMV
    !---Open file 
    iu=get_lun()
    OPEN(iu,file=OutputFile,form='unformatted')
    WRITE(iu) SceneAMV%iTyp,SceneAMV%AlgSN
    WRITE(iu) nprf
    WRITE(iu) SceneAMV%nqc
    RETURN
  END SUBROUTINE WriteHdrSceneAMV



!===============================================================
! Name:         WriteSceneAMV
!
!
! Type:         Subroutine
!
!
! Description:  Writes the scene content (not header) into a file
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - iu                I               Unit number
!       - SceneAMV             I               SceneAMV structure whose
!                                           content is to be written out
!
!
! Modules needed:
!       - None
!
!
! History:
!       04-02-2014      Eric S. Maddy, RTi
!
!===============================================================

  SUBROUTINE WriteSceneAMV(iu,SceneAMV)
    TYPE(SceneAMV_type)   :: SceneAMV
    INTEGER            :: iu

    WRITE(iu) SceneAMV%DescAlg,SceneAMV%ProfIndx, SceneAMV%WindCalcMethod, SceneAMV%HeightMethod
    !---Atmospheric constituents
    !---QC
    !---Positioning variables
    WRITE(iu) SceneAMV%angle,SceneAMV%WindSp,SceneAMV%WindDir,SceneAMV%WindPress,SceneAMV%WindU,SceneAMV%WindV,SceneAMV%WindError, SceneAMV%qi(1:SceneAMV%nqc), SceneAMV%qc(1:SceneAMV%nqc), SceneAMV%lat,SceneAMV%lon,SceneAMV%node,SceneAMV%scanUTC,SceneAMV%scanYear,SceneAMV%scanDay,SceneAMV%scanMonth,SceneAMV%iscanPos,SceneAMV%iscanLine
    RETURN
  END SUBROUTINE WriteSceneAMV


!===============================================================
! Name:         ReadSceneAMV
!
!
! Type:         Subroutine
!
!
! Description:  Reads content of a geophysical scene file.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - iu                 I              Unit number
!       - SceneAMV              I/O            Structure to be filled 
!                                           with content of file
!       - ierr               O              Error index when reading
!
!
! Modules needed:
!       - None
!
!
! History:
!       04-02-2014      Eric S. Maddy, RTi
!
!===============================================================

  SUBROUTINE ReadSceneAMV(iu,SceneAMV,ierr)
    TYPE(SceneAMV_type)   :: SceneAMV
    INTEGER            :: iu,ierr
    ierr=0
    READ(iu,iostat=ierr,end=10) SceneAMV%DescAlg,SceneAMV%ProfIndx, SceneAMV%WindCalcMethod, SceneAMV%HeightMethod
    IF (ierr.ne.0) THEN
       ierr=Warn_readInvalid
       CALL ErrHandl(WarningType,Warn_readInvalid,'SceneAMV invalid.')
       RETURN
    ENDIF
    !---Wind parameters
    READ(iu,err=20) SceneAMV%angle,SceneAMV%WindSp,SceneAMV%WindDir, SceneAMV%WindPress, &
         SceneAMV%WindU,SceneAMV%WindV,SceneAMV%WindError, &   
         SceneAMV%qi(1:SceneAMV%nqc), SceneAMV%qc(1:SceneAMV%nqc), & !---QC variables 
         SceneAMV%lat,SceneAMV%lon,SceneAMV%node,SceneAMV%scanUTC,& 
         SceneAMV%scanYear,SceneAMV%scanDay,SceneAMV%scanMonth,&
         SceneAMV%iscanPos,SceneAMV%iscanLine !---Positioning variables

!!$    !---Surface-level paremeters
!!$    READ(iu,err=20) SceneAMV%angle,SceneAMV%WindSp,SceneAMV%WindDir, SceneAMV%WindPress,SceneAMV%WindU,SceneAMV%WindV 
!!$    !---QC variables 
!!$    READ(iu,err=20) SceneAMV%qc(1:SceneAMV%nqc)
!!$    !---Positioning variables
!!$
!!$    READ(iu,err=20) SceneAMV%lat,SceneAMV%lon,SceneAMV%node,SceneAMV%scanUTC,SceneAMV%scanYear,SceneAMV%scanDay,SceneAMV%scanMonth,SceneAMV%iscanPos,SceneAMV%iscanLine 

    RETURN
10  CONTINUE
    ierr=Warn_EndOfFile
    CALL ErrHandl(WarningType,Warn_EndOfFile,'SceneAMV') 
    RETURN
20  ierr=Warn_readInvalid
    CALL ErrHandl(WarningType,Warn_readInvalid,'(ReadSceneAMV)')
    RETURN

  END SUBROUTINE ReadSceneAMV


!===============================================================
! Name:         SetUpSceneAMV
!
!
! Type:         Subroutine
!
!
! Description:  This subroutine basically sets up the scene 
!               structure with elements coming from the inputs.
!               This is to be done before writing the scene into
!               a file.
!
!
! Arguments:
!
!           Name                    Type            Description
!      ---------------------------------------------------
!       - angle                      I           Viewing angle
!       - SceneAMV                      I/O         Structure to be set
!       - iProf                      I           Profile number
!       - lat                        I           Latitude
!       - lon                        I           Longitude
!       - node                       I           Ascending/descending mode
!       - scanDAY                    I           Day of the measurement
!       - scanYear                   I           Year of the measurement
!       - scanUTC                    I           UTC time of the measurement
!       - iscanPos                   I           Scan position of measurem.
!       - qc                         I           QC vector (if any)
!
!
! Modules needed:
!       - None
!
!
! History:
!       04-02-2014      Eric S. Maddy, RTi
!
!===============================================================

  SUBROUTINE SetUpSceneAMV(pres_lev,pres_lay,angle,SceneAMV,iProf,lat,lon,node,&
       scanDAY,scanYear,scanMonth,scanUTC,iscanPos,iscanLine,RelAziAngle,SolZenAngle)
    REAL,              DIMENSION(:) :: pres_lev,pres_lay
    REAL                            :: angle,RelAziAngle,SolZenAngle
    REAL                            :: lat,lon,scanUTC
    TYPE(SceneAMV_type)                :: SceneAMV
    INTEGER                         :: iProf,node,scanDay,scanYear,scanMonth,iscanPos,iscanLine

    SceneAMV%ProfIndx                      = iProf
    SceneAMV%angle                         = angle
    SceneAMV%lat                           = lat
    SceneAMV%lon                           = lon
    SceneAMV%node                          = node
    SceneAMV%scanDay                       = scanDay
    SceneAMV%scanYear                      = scanYear
    SceneAMV%scanMonth                     = scanMonth
    SceneAMV%scanUTC                       = scanUTC
    SceneAMV%iscanPos                      = iscanPos
    SceneAMV%iscanLine                     = iscanLine
    RETURN
  END SUBROUTINE SetUpSceneAMV

!===============================================================
! Name:         DestroySceneAMV
!
!
! Type:         Subroutine
!
!
! Description:  Deallocate scene
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - SceneAMV              I/O            Structure to be deallocated
!
!
! Modules needed:
!       - None
!
!
! History:
!       04-02-2014      Eric S. Maddy, RTi
!
!===============================================================

  SUBROUTINE DestroySceneAMV(SceneAMV)
    TYPE(SceneAMV_type)   :: SceneAMV
    DEALLOCATE(SceneAMV%qc)
    DEALLOCATE(SceneAMV%qi)

  END SUBROUTINE DestroySceneAMV

!===============================================================
! Name:         PrintSceneAMV
!
!
! Type:         Subroutine
!
!
! Description:  Log-prints the scene contents 
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - SceneAMV              I            SceneAMV structure (see 
!                                         definition in top 
!                                         section of module)
!
!
! Modules needed:
!       - None
!
!
! History:
!       04-02-2014      Eric S. Maddy RTi  Created 
!
!===============================================================

  SUBROUTINE PrintSceneAMV(SceneAMV)
    TYPE(SceneAMV_type)                :: SceneAMV    
    REAL                            :: xtpw1,iclw,rwp,iwp,gwp,swp
!!$    print *, 'Algorithm serial number:',SceneAMV%AlgSN
!!$    print *, 'TypAtm:',SceneAMV%iTypAtm,SceneAMV%DescTypAtm
!!$    print *, 'TypSfc:',SceneAMV%iTypSfc,SceneAMV%DescTypSfc
!!$    print *, 'Nlev/Nlay:',SceneAMV%Nlev,SceneAMV%nLay
!!$    print *, 'Nabsorb:',SceneAMV%Nabsorb
!!$    print *, 'AborbIDs:',SceneAMV%AbsorbID
!!$    print *, 'H2O Indx:',SceneAMV%iH2O
!!$    !print *, 'O3 Indx:',SceneAMV%iO3
!!$    print *, 'Pres(Lev):',SceneAMV%Pres_lev
!!$    print *, 'Pres(Lay):',SceneAMV%Pres_lay
!!$    print *, 'Temp(Lay):',SceneAMV%Temp_lay
!!$    print *, 'nPar(CLW/Rain/Snow):',SceneAMV%nParmCLW,SceneAMV%nParmRain,SceneAMV%nParmSnow
!!$    print *, 'nPar(Ice/Grpl):',SceneAMV%nParmIce,SceneAMV%nParmGrpl
!!$    print *, 'Absorbers(Lay) H2O:',SceneAMV%Absorb_lay(:,SceneAMV%iH2O)
!!$    !print *, 'Absorbers(Lay) O3:',SceneAMV%Absorb_lay(:,SceneAMV%iO3)
!!$    print *, 'CLW:',SceneAMV%Clw
!!$    print *, 'Rain:',SceneAMV%Rain
!!$    !print *, 'Snow:',SceneAMV%Snow
!!$    !print *, 'Ice:',SceneAMV%Ice
!!$    print *, 'Graupel:',SceneAMV%Graupel
!!$    call ComputeTPW(SceneAMV%pres_lev(1:SceneAMV%nLev),&
!!$         SceneAMV%SfcPress,SceneAMV%Absorb_lay(1:SceneAMV%nLay,SceneAMV%iH2O),xtpw1)
!!$    iclw=ColumIntegr(SceneAMV%nParmCLW,SceneAMV%pres_lev(1:SceneAMV%nLev),SceneAMV%SfcPress,SceneAMV%clw(1:SceneAMV%nParmCLW))
!!$    gwp=ColumIntegr(SceneAMV%nParmGrpl,SceneAMV%pres_lev(1:SceneAMV%nLev),SceneAMV%SfcPress,SceneAMV%graupel(1:SceneAMV%nParmGrpl))
!!$    rwp=ColumIntegr(SceneAMV%nParmRain,SceneAMV%pres_lev(1:SceneAMV%nLev),SceneAMV%SfcPress,SceneAMV%rain(1:SceneAMV%nParmRain))
!!$    swp=ColumIntegr(SceneAMV%nParmSnow,SceneAMV%pres_lev(1:SceneAMV%nLev),SceneAMV%SfcPress,SceneAMV%snow(1:SceneAMV%nParmSnow))
!!$    iwp=ColumIntegr(SceneAMV%nParmIce,SceneAMV%pres_lev(1:SceneAMV%nLev),SceneAMV%SfcPress,SceneAMV%ice(1:SceneAMV%nParmIce))
!!$    print *, 'TPW:',xtpw1
!!$    print *, 'CLW:',iClw
!!$    print *, 'Rain:',rwp
!!$    print *, 'Snow:',swp
!!$    print *, 'Ice:',iwp
!!$    print *, 'Graupel:',gwp
!!$    print *, 'nChan:',SceneAMV%nchan
!!$    print *, 'Freq:',SceneAMV%CentrFreq
!!$    print *, 'Polar:',SceneAMV%polarity
!!$    print *, 'Angle:',SceneAMV%Angle
!!$    print *, 'Rel-Azi Angle:',SceneAMV%RelAziAngle
!!$    print *, 'Solar Zenith Angle:',SceneAMV%SolZenAngle
!!$    print *, 'Emiss:',SceneAMV%Emiss
!!$    print *, 'Refl:',SceneAMV%Refl
!!$    print *, 'WindSp:',SceneAMV%WindSp
!!$    print *, 'WindDir:',SceneAMV%WindDir
!!$    print *, 'Tskin:',SceneAMV%Tskin
!!$    print *, 'SnowDepth:',SceneAMV%snowdepth
!!$    print *, 'SfcPres:',SceneAMV%SfcPress
!!$    print *, '----------------END of profile---------------'
    RETURN
  END SUBROUTINE PrintSceneAMV

END MODULE IO_SceneAMV

