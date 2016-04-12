!===============================================================
! Name:    sceneDump.f90
!
!
! Type:    Main Program
!
!
! Description:
!       Program that collocates the NWP gridded data into an
!       orbit-based space using radiance measurements.
!
!
! Modules needed:
!       - CRTM_Module
!       - MWwaterCoeff_Define
!       - CRTM_MWwaterCoeff
!       - misc
!       - Consts
!       - utils
!       - IO_MeasurData
!       - IO_Scene
!       - IO_Misc
!       - GeophCovBkg
!       - ErrorHandling
!       - Preclassif
!
!
! History:
!        01/21/2015, ESM, Added emissivity and analytic emissivity to dump file
!
!===============================================================

program sceneDump

  USE Consts
  USE misc
  USE utils
  USE IO_Scene
  USE IO_Misc
  USE ErrorHandling

  implicit none

  !---INTRINSIC functions used in this module
  INTRINSIC :: ADJUSTL,COUNT,INDEX,INT,MAXVAL,MINVAL,PACK,REAL,SIZE,SQRT,TRIM,ALL,MOD,LEN_TRIM
  !---Different parameters
  INTEGER            :: sfcTypeIdx
  INTEGER, PARAMETER :: len=256
  INTEGER            :: len_file=0

  !---Pointers and other arrays
  CHARACTER(LEN=len), DIMENSION(:),       POINTER     :: SceneFiles, OutFiles
  !---Single variables
  CHARACTER(LEN=10)  :: OutFilePrefix, OutFileSuffix
  INTEGER            :: iu,iuOut,indx,i,ierr,Error_status,allocate_status
  INTEGER            :: nfile,ifile,nprofiles,iprof, iu_listrad, nchan
  !---NWP-related scene data
  TYPE(Scene_type)                      :: Scene    
  REAL                                  :: clw, tpw, rwp, gwp, versid 
  !---Namelist data 
  CHARACTER(LEN=len) :: SceneFileList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: pathout=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: LogFile=DEFAULT_VALUE_STR4
  
  NAMELIST /ControlSD/SceneFileList,pathout,LogFile

  !-----------------------------------------------------
  !     Read control-data from namelist
  !-----------------------------------------------------
  READ(*,NML=ControlSD)
  !---Prepare Log file
  CALL OpenLogFile(Logfile)
  OutFilePrefix = 'Scenedump'
  OutFileSuffix = ''
  versid = 1.01
  iu_listrad = 10
  iu = 11
  iuOut = 12
  !---Read the file names of radiance data and build output NWP files names
  call ReadList4(iu_listrad,trim(SceneFileList),SceneFiles,nfile,OutFiles,pathout,trim(OutFilePrefix),trim(OutFileSuffix))
  IF (nfile .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(scene files in dump)')
  CLOSE(iu_listrad)
  !-----------------------------------------------------
  !     Loop over the scene files
  !-----------------------------------------------------
  clw = 0.
  rwp = 0.
  gwp = 0.
  tpw = 0.
  FilesLoop: DO ifile=1,nfile

     !---Read header of adjusted scene
     CALL ReadHdrScene(iu,SceneFiles(ifile),Scene,nProfiles)
     OPEN(unit=iuOut,FILE=Outfiles(ifile),FORM='UNFORMATTED')
     nchan = Scene%nchan
     WRITE(iuOut) versid, nProfiles, nchan
!     print *, versid, nProfiles
     ProfLoop: DO iprof=1,nprofiles
        !-------------------------------------------
        !---Read scene
        !-------------------------------------------
        CALL ReadScene(iu,Scene,ierr)
        IF (ierr.ne.0) THEN 
           CALL ErrHandl(ierr,Warn_EndOfFile,'(scene files in dump)')
        ENDIF
        !-------------------------------------------
        !---Output information required by assessment tool
        !-------------------------------------------
        clw = ColumIntegr(Scene%nParmCLW,Scene%Pres_Lev(1:Scene%nlev),Scene%SfcPress,Scene%CLW(1:Scene%nParmCLW))   
        rwp = ColumIntegr(Scene%nParmRain,Scene%Pres_Lev(1:Scene%nlev),Scene%SfcPress,Scene%Rain(1:Scene%nParmRain))   
        gwp = ColumIntegr(Scene%nParmGrpl,Scene%Pres_Lev(1:Scene%nlev),Scene%SfcPress,Scene%Graupel(1:Scene%nParmGrpl))   
        CALL ComputeTPW(Scene%Pres_Lev(1:Scene%nlev),Scene%SfcPress,&
             Scene%Absorb_lay(1:Scene%nLay,Scene%iH2o),tpw)  
        WRITE(iuOut) clw, rwp, gwp, tpw, Scene%Tskin, Scene%itypSfc, Scene%SfcPress, &
             scene%windU, scene%windV
        WRITE(iuOut) Scene%emiss(1:nChan), Scene%EmissAnlytc(1:nChan)
!        PRINT *, iprof, ierr, clw, rwp, gwp, tpw, Scene%Tskin, Scene%itypSfc, Scene%SfcPress   
        
      ENDDO ProfLoop
      CALL DestroyScene(Scene)
      CLOSE(iuOut)
      CLOSE(iu)
  ENDDO FilesLoop
  
  CALL CloseLogFile()

end program scenedump
