MODULE IO_Misc_LE
USE misc
USE utils


IMPLICIT NONE
PRIVATE
!---Publicly available subroutine
PUBLIC :: readECMWFanalys

CONTAINS

!==================================================================================
!
! History:
!       03-27-2008      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!       04-11-2014      Fixed bug for 137 level model update 
!                       Eric S. Maddy, RTi @ NOAA/NESDIS/STAR 
!      http://www.ecmwf.int/products/changes/ifs_cycle_38r2/level_comp_91_137.html 
!
!===================================================================================

  SUBROUTINE readECMWFanalys(sfcECMWFile,atmECMWFile,ECMWFieldsR,&
       nparam,iflag,use138)
   CHARACTER(LEN=*)                  :: sfcECMWFile,atmECMWFile
   INTEGER                           :: iu,iflag,status
   INTEGER                           :: nparam,nREC
   INTEGER                           :: mxi,myi,nxi,nyi,nx,ny
   INTEGER                           :: nxp,nyp,nlev,nlevuse
   LOGICAL, OPTIONAL                 :: use138
   REAL                              :: fieldadd,CF
   REAL                              :: sfctype
   REAL                              :: HPO, HP1, ECMAO, ECMBO        
   REAL                              :: ECMA1, ECMB1        
   REAL,  DIMENSION(92)              :: ECMWFA=&
        (/0.000000,2.000040,3.980832,7.387186,12.908319,21.413612,33.952858,51.746601, & 
        76.167656,108.715561,150.986023,204.637451,271.356506,352.824493,450.685791, &
        566.519226,701.813354,857.945801,1036.166504,1237.585449,1463.163940,1713.709595, & 
        1989.874390,2292.155518,2620.898438,2976.302246,3358.425781,3767.196045,4202.416504, & 
        4663.776367,5150.859863,5663.156250,6199.839355,6759.727051,7341.469727,7942.926270, & 
        8564.624023,9208.305664,9873.560547,10558.881836,11262.484375,11982.662109,12713.897461, & 
        13453.225586,14192.009766,14922.685547,15638.053711,16329.560547,16990.623047,17613.281250, & 
        18191.029297,18716.968750,19184.544922,19587.513672,19919.796875,20175.394531,20348.916016, & 
        20434.158203,20426.218750,20319.011719,20107.031250,19785.357422,19348.775391,18798.822266, & 
        18141.296875,17385.595703,16544.585938,15633.566406,14665.645508,13653.219727,12608.383789, & 
        11543.166992,10471.310547,9405.222656,8356.252930,7335.164551,6353.920898,5422.802734, & 
        4550.215820,3743.464355,3010.146973,2356.202637,1784.854614,1297.656128,895.193542, & 
        576.314148,336.772369,162.043427,54.208336,6.575628,0.003160,0.000000/)
   REAL,  DIMENSION(92)              :: ECMWFB=&
        (/0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000, &
         0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000, & 
         0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000, &
         0.000000,0.000000,0.000000,0.000000,0.000000,0.000014,0.000055,0.000131,0.000279,0.000548, &
         0.001000,0.001701,0.002765,0.004267,0.006322,0.009035,0.012508,0.016860,0.022189,0.028610, &
         0.036227,0.045146,0.055474,0.067316,0.080777,0.095964,0.112979,0.131935,0.152934,0.176091, &
         0.201520,0.229315,0.259554,0.291993,0.326329,0.362203,0.399205,0.436906,0.475016,0.513280, &
         0.551458,0.589317,0.626559,0.662934,0.698224,0.732224,0.764679,0.795385,0.824185,0.850950, &
         0.875518,0.897767,0.917651,0.935157,0.950274,0.963007,0.973466,0.982238,0.989153,0.994204, &
         0.997630,1.000000/)

   REAL,  DIMENSION(138)              :: ECMWFA_138=&
        (/ 0.000000, 2.000365, 3.102241, 4.666084, 6.827977, 9.746966,13.605424,&
        18.608931,24.985718,32.985710,42.879242,54.955463,69.520576,86.895882,&
        107.415741,  131.425507,  159.279404,  191.338562,  227.968948,  269.539581,  316.420746,&
        368.982361,  427.592499,  492.616028,  564.413452,  643.339905,  729.744141,  823.967834,&
        926.344910, 1037.201172, 1156.853638, 1285.610352, 1423.770142, 1571.622925, 1729.448975,&
        1897.519287, 2076.095947, 2265.431641, 2465.770508, 2677.348145, 2900.391357, 3135.119385,&
        3381.743652, 3640.468262, 3911.490479, 4194.930664, 4490.817383, 4799.149414, 5119.895020,&
        5452.990723, 5798.344727, 6156.074219, 6526.946777, 6911.870605, 7311.869141, 7727.412109,&
        8159.354004, 8608.525391, 9076.400391, 9562.682617,10065.978516,10584.631836,11116.662109,&
        11660.067383,12211.547852,12766.873047,13324.668945,13881.331055,14432.139648,14975.615234,&
        15508.256836,16026.115234,16527.322266,17008.789062,17467.613281,17901.621094,18308.433594,&
        18685.718750,19031.289062,19343.511719,19620.042969,19859.390625,20059.931641,20219.664062,&
        20337.863281,20412.308594,20442.078125,20425.718750,20361.816406,20249.511719,20087.085938,&
        19874.025391,19608.572266,19290.226562,18917.460938,18489.707031,18006.925781,17471.839844,&
        16888.687500,16262.046875,15596.695312,14898.453125,14173.324219,13427.769531,12668.257812,&
        11901.339844,11133.304688,10370.175781, 9617.515625, 8880.453125, 8163.375000, 7470.343750,&
        6804.421875, 6168.531250, 5564.382812, 4993.796875, 4457.375000, 3955.960938, 3489.234375,&
        3057.265625, 2659.140625, 2294.242188, 1961.500000, 1659.476562, 1387.546875, 1143.250000,&
        926.507812,  734.992188,  568.062500,  424.414062,  302.476562,  202.484375,  122.101562,&
        62.781250,22.835938, 3.757813, 0.000000, 0.000000/)
   REAL,  DIMENSION(138)              :: ECMWFB_138=&
        (/ 0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,&
        0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,&
        0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,&
        0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,&
        0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,&
        0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,&
        0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,&
        0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000007,&
        0.000024,0.000059,0.000112,0.000199,0.000340,0.000562,0.000890,&
        0.001353,0.001992,0.002857,0.003971,0.005378,0.007133,0.009261,&
        0.011806,0.014816,0.018318,0.022355,0.026964,0.032176,0.038026,&
        0.044548,0.051773,0.059728,0.068448,0.077958,0.088286,0.099462,&
        0.111505,0.124448,0.138313,0.153125,0.168910,0.185689,0.203491,&
        0.222333,0.242244,0.263242,0.285354,0.308598,0.332939,0.358254,&
        0.384363,0.411125,0.438391,0.466003,0.493800,0.521619,0.549301,&
        0.576692,0.603648,0.630036,0.655736,0.680643,0.704669,0.727739,&
        0.749797,0.770798,0.790717,0.809536,0.827256,0.843881,0.859432,&
        0.873929,0.887408,0.899900,0.911448,0.922096,0.931881,0.940860,&
        0.949064,0.956550,0.963352,0.969513,0.975078,0.980072,0.984542,&
        0.988500,0.991984,0.995003,0.997630, 1.000000/)
   !--- index of the 91 sublevels in the 138 level grid 
   INTEGER, DIMENSION(91)             :: LEVEL_138=&
        (/  1,  2,  4,  6,  7,  9, 10, 12, 14, 15,&
        17, 19, 20, 22, 24, 25, 27, 29, 30, 32,&
        34, 35, 37, 38, 40, 42, 43, 45, 46, 48,&
        49, 51, 52, 54, 55, 57, 58, 59, 61, 62,&
        63, 65, 66, 67, 69, 70, 71, 73, 74, 76,&
        77, 78, 80, 81, 82, 84, 85, 87, 88, 90,&
        91, 93, 94, 96, 97, 98,100,101,103,104,&
        105,107,108,110,111,112,114,116,117,119,&
        120,122,124,126,128,129,131,133,134,136,&
        137/)
   
   REAL,  DIMENSION(1440,721)            :: Pressure     
   REAL,  DIMENSION(1440,721)            :: ECMWFields   
   REAL,  DIMENSION(:,:,:)               :: ECMWFieldsR  
  

!=============================================================
!Reading the surface parameters  
!=============================================================

 IF (iflag==1) THEN
    iu=get_lun()
    !Opening the file containing the surface parameters
    OPEN (UNIT=iu, FILE=sfcECMWFile, ACCESS= 'DIRECT', STATUS='OLD',& 
           ACTION='READ', FORM='UNFORMATTED', RECL=1440*721*4, IOSTAT=status)
    DO nREC=1,nparam
       READ(iu, IOSTAT=status, REC=nREC ) ECMWFields(:,:) 
       IF (nREC .eq. 2) THEN 
          CF=0.01
       ELSE  
          CF=1.0
       ENDIF

       !Regriding surface parameters from 1440x721 to 360x181 points
       DO myi=0,(720/4)-1
          DO mxi=0,(1440/4)-1  
             fieldadd=0
             DO nyi= 1,4
                DO nxi= 1,4         
                   fieldadd=ECMWFields(nxi+4*mxi,nyi+4*myi) + fieldadd
                ENDDO
             ENDDO
             ECMWFieldsR(myi+1,mxi+1,nREC)= CF*fieldadd/16                     
          ENDDO
       ENDDO

     
       DO mxi=0,(1440/4)-1   
          fieldadd=0
          DO nxi= 1,4
             fieldadd=ECMWFields(nxi+4*mxi,721) + fieldadd
          ENDDO
          ECMWFieldsR(181,mxi+1,nREC)= CF*fieldadd/4 
       ENDDO
    
    ENDDO
    CLOSE(iu) !Closing the file containing the surface parameters


!--Defining the surface type
    DO ny=1,181
       DO nx=1,360
          sfctype=ECMWFieldsR(ny,nx,1)
          IF ( sfctype >= 0.0 .AND. sfctype <= 0.05 ) THEN
             ECMWFieldsR(ny,nx,1)= 0.0
          ELSE IF (sfctype >= 0.95 .AND. sfctype <= 1.0 ) THEN
             ECMWFieldsR(ny,nx,1)= 2.0
          ELSE 
             ECMWFieldsR(ny,nx,1)= 6.0
          ENDIF
       ENDDO
    ENDDO
     
!=============================================================
!Reading the atmospheric parameters  
!=============================================================
 
 ELSE IF (iflag==2) THEN

    iu=get_lun()
    !Openin the file containing the atmospheric parameters
    OPEN (UNIT=iu, FILE=atmECMWFile, ACCESS= 'DIRECT', STATUS='OLD',& 
         ACTION='READ', FORM='UNFORMATTED', RECL=1440*721*4, IOSTAT=status)

    DO nREC=1,nparam-91
       READ(iu, IOSTAT=status, REC=nREC ) ECMWFields(:,:)

       IF (nREC <= 92) THEN 
          CF=1
       ELSE 
          CF=1000
       ENDIF

       !Regriding atmosheric parameters from 1440x721 to 360x181 points
       DO myi=0,(720/4)-1
          DO mxi=0,(1440/4)-1  
             fieldadd=0
             DO nyi= 1,4
                DO nxi= 1,4         
                   fieldadd=ECMWFields(nxi+4*mxi,nyi+4*myi) + fieldadd
                ENDDO
             ENDDO
             ECMWFieldsR(myi+1,mxi+1,nREC)= CF*fieldadd/16           
          ENDDO
       ENDDO

       DO mxi=0,(1440/4)-1   
          fieldadd=0
          DO nxi= 1,4
             fieldadd=ECMWFields(nxi+4*mxi,721) + fieldadd
          ENDDO
          ECMWFieldsR(181,mxi+1,nREC)= CF*fieldadd/4
       ENDDO
    ENDDO
    CLOSE(iu) !Closing the file containing the atmospheric parameters

    !--Defining the surface type
    DO ny=1,181
       DO nx=1,360
          sfctype=ECMWFieldsR(ny,nx,1)
          IF ( sfctype >= 0.0 .AND. sfctype <= 0.05 ) THEN
             ECMWFieldsR(ny,nx,1)= 0.0
          ELSE IF (sfctype >= 0.95 .AND. sfctype <= 1.0 ) THEN
             ECMWFieldsR(ny,nx,1)= 2.0
          ELSE 
             ECMWFieldsR(ny,nx,1)= 6.0
          ENDIF
       ENDDO
    ENDDO
 
!--------------------------------------------------------------------------------
!  Calculating the Pressure Field using the surface pressure and the ECMWF model 
!  level definition
!--------------------------------------------------------------------------------
    iu=get_lun()
    !Opening the file with the surface parameters
    OPEN (UNIT=iu, FILE=sfcECMWFile, ACCESS= 'DIRECT', STATUS='OLD',& 
         ACTION='READ', FORM='UNFORMATTED', RECL=1440*721*4, IOSTAT=status)
    !The second grid layer of the sfcECMWF file  must contain the surface pressure
    READ(iu, IOSTAT=status, REC=2 ) ECMWFields(:,:)
    CLOSE(iu) !Closing the file containing the surface parameters

    !Calculation of the Pressure Field in mbars
    DO nlev=1, 91   
       nlevuse = nlev
       ECMAO=ECMWFA(nlevuse) 
       ECMBO=ECMWFB(nlevuse) 
       ECMA1=ECMWFA(nlevuse+1) 
       ECMB1=ECMWFB(nlevuse+1) 
       IF (PRESENT(use138)) THEN
          IF (use138) THEN 
            nlevuse = level_138(nlev)
            ECMAO=ECMWFA_138(nlevuse) 
            ECMBO=ECMWFB_138(nlevuse) 
            ECMA1=ECMWFA_138(nlevuse+1) 
            ECMB1=ECMWFB_138(nlevuse+1) 
          ENDIF
       ENDIF
       DO nyp=1, 721
          DO nxp=1, 1440
             HPO = ECMAO + ECMBO*ECMWFields(nxp,nyp)
             HP1 = ECMA1 + ECMB1*ECMWFields(nxp,nyp)
             Pressure(nxp,nyp)= 0.01 * ( HPO + HP1) * 0.5
          ENDDO
       ENDDO
       !Regrid the Pressure Field from 1440x721 to 360x181 points
       DO myi=0,(720/4)-1
          DO mxi=0,(1440/4)-1  
             fieldadd=0
             DO nyi= 1,4
                DO nxi= 1,4         
                   fieldadd=Pressure(nxi+4*mxi,nyi+4*myi) + fieldadd
                ENDDO
             ENDDO
             ECMWFieldsR(myi+1,mxi+1,nlev+456)= fieldadd/16       
          ENDDO
       ENDDO
       DO mxi=0,(1440/4)-1   
          fieldadd=0
          DO nxi= 1,4
             fieldadd=Pressure(nxi+4*mxi,721) + fieldadd
          ENDDO
          ECMWFieldsR(181,mxi+1,nlev+456)= fieldadd/4
       ENDDO
    ENDDO
!--------------------------------------------------------------------------------------
!  Converting Specific Humidity to Mixing Ratio in g/kg
!--------------------------------------------------------------------------------------      
!    DO nlev=1,91      
!       DO ny=1,181
!          DO nx=1,360
!             ECMWFieldsR(ny,nx,nlev+92) = ECMWFieldsR(ny,nx,nlev+92)/( 1 - 0.001*ECMWFieldsR(ny,nx,nlev+92) )
!          ENDDO
!       ENDDO
!    ENDDO
       
         
 ELSE IF (iflag==3) THEN

    iu=get_lun()
    !Openin the file containing the atmospheric parameters
    OPEN (UNIT=iu, FILE=atmECMWFile, ACCESS= 'DIRECT', STATUS='OLD',& 
         ACTION='READ', FORM='UNFORMATTED', RECL=1440*721*4, IOSTAT=status)

    DO nREC=1,nparam-91
       READ(iu, IOSTAT=status, REC=nREC ) ECMWFields(:,:)
       
       CF=1

       !Regriding atmosheric parameters from 1440x721 to 360x181 points
       DO myi=0,(720/4)-1
          DO mxi=0,(1440/4)-1  
             fieldadd=0
             DO nyi= 1,4
                DO nxi= 1,4         
                   fieldadd=ECMWFields(nxi+4*mxi,nyi+4*myi) + fieldadd
                ENDDO
             ENDDO
             ECMWFieldsR(myi+1,mxi+1,nREC)= CF*fieldadd/16           
          ENDDO
       ENDDO

       DO mxi=0,(1440/4)-1   
          fieldadd=0
          DO nxi= 1,4
             fieldadd=ECMWFields(nxi+4*mxi,721) + fieldadd
          ENDDO
          ECMWFieldsR(181,mxi+1,nREC)= CF*fieldadd/4
       ENDDO
    ENDDO
    CLOSE(iu) !Closing the file containing the atmospheric parameters

    !--Defining the surface type
    DO ny=1,181
       DO nx=1,360
          sfctype=ECMWFieldsR(ny,nx,1)
          IF ( sfctype >= 0.0 .AND. sfctype <= 0.05 ) THEN
             ECMWFieldsR(ny,nx,1)= 0.0
          ELSE IF (sfctype >= 0.95 .AND. sfctype <= 1.0 ) THEN
             ECMWFieldsR(ny,nx,1)= 2.0
          ELSE 
             ECMWFieldsR(ny,nx,1)= 6.0
          ENDIF
       ENDDO
    ENDDO
 
!--------------------------------------------------------------------------------
!  Calculating the Pressure Field using the surface pressure and the ECMWF model 
!  level definition
!--------------------------------------------------------------------------------
    iu=get_lun()
    !Opening the file with the surface parameters
    OPEN (UNIT=iu, FILE=sfcECMWFile, ACCESS= 'DIRECT', STATUS='OLD',& 
         ACTION='READ', FORM='UNFORMATTED', RECL=1440*721*4, IOSTAT=status)
    !The second grid layer of the sfcECMWF file  must contain the surface pressure
    READ(iu, IOSTAT=status, REC=2 ) ECMWFields(:,:)
    CLOSE(iu) !Closing the file containing the surface parameters

    !Calculation of the Pressure Field in mbars
    DO nlev=1, 91   
       nlevuse = nlev
       ECMAO=ECMWFA(nlevuse) 
       ECMBO=ECMWFB(nlevuse) 
       ECMA1=ECMWFA(nlevuse+1) 
       ECMB1=ECMWFB(nlevuse+1) 
       IF (PRESENT(use138)) THEN
          IF (use138) THEN 
            nlevuse = level_138(nlev)
            ECMAO=ECMWFA_138(nlevuse) 
            ECMBO=ECMWFB_138(nlevuse) 
            ECMA1=ECMWFA_138(nlevuse+1) 
            ECMB1=ECMWFB_138(nlevuse+1) 
          ENDIF
       ENDIF
       DO nyp=1, 721
          DO nxp=1, 1440
             HPO = ECMAO + ECMBO*ECMWFields(nxp,nyp)
             HP1 = ECMA1 + ECMB1*ECMWFields(nxp,nyp)
             Pressure(nxp,nyp)= 0.01 * ( HPO + HP1) * 0.5
          ENDDO
       ENDDO
       !Regrid the Pressure Field from 1440x721 to 360x181 points
       DO myi=0,(720/4)-1
          DO mxi=0,(1440/4)-1  
             fieldadd=0
             DO nyi= 1,4
                DO nxi= 1,4         
                   fieldadd=Pressure(nxi+4*mxi,nyi+4*myi) + fieldadd
                ENDDO
             ENDDO
             ECMWFieldsR(myi+1,mxi+1,nlev+456)= fieldadd/16       
          ENDDO
       ENDDO
       DO mxi=0,(1440/4)-1   
          fieldadd=0
          DO nxi= 1,4
             fieldadd=Pressure(nxi+4*mxi,721) + fieldadd
          ENDDO
          ECMWFieldsR(181,mxi+1,nlev+456)= fieldadd/4
       ENDDO
    ENDDO
    !Calculation of the Pressure Field in mbars
!!$    DO nlev=1, 91   
!!$       DO nyp=1, 721
!!$          DO nxp=1, 1440
!!$             HPO=ECMWFA(nlev) + ECMWFB(nlev)*ECMWFields(nxp,nyp)
!!$             HP1=ECMWFA(nlev+1) + ECMWFB(nlev+1)*ECMWFields(nxp,nyp)
!!$             Pressure(nxp,nyp)= 0.01 * ( HPO + HP1) * 0.5
!!$          ENDDO
!!$       ENDDO
!!$       !Regrid the Pressure Field from 1440x721 to 360x181 points
!!$       DO myi=0,(720/4)-1
!!$          DO mxi=0,(1440/4)-1  
!!$             fieldadd=0
!!$             DO nyi= 1,4
!!$                DO nxi= 1,4         
!!$                   fieldadd=Pressure(nxi+4*mxi,nyi+4*myi) + fieldadd
!!$                ENDDO
!!$             ENDDO
!!$             ECMWFieldsR(myi+1,mxi+1,nlev+456)= fieldadd/16       
!!$          ENDDO
!!$       ENDDO
!!$       DO mxi=0,(1440/4)-1   
!!$          fieldadd=0
!!$          DO nxi= 1,4
!!$             fieldadd=Pressure(nxi+4*mxi,721) + fieldadd
!!$          ENDDO
!!$          ECMWFieldsR(181,mxi+1,nlev+456)= fieldadd/4
!!$       ENDDO
!!$    ENDDO

        
 ELSE
    print*,'Neither surface nor atmospheric nor wind field parameter has been read'

 ENDIF



END SUBROUTINE readECMWFanalys



END MODULE IO_Misc_LE
