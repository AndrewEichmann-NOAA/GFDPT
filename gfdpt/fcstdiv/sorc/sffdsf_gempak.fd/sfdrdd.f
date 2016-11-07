        SUBROUTINE SFDRDD ( lunin, frmt, nprmsf, parms, npgiv, stid, 
     +			     istnm, slat, slon, selv, sfdata, iret )
C************************************************************************
C* SFDRDD								*
C*									*
C* This subroutine reads the next record from a surface text formatted	*	
C* dataset.								*
C*									*
C* SFDRDD  ( LUNIN, FRMT, NPRMSF, PARMS, STID, ISTNM, SLAT, SLON, SELV,	*
C*	     SFDATA, IRET )						*
C*									*
C* Input parameters:							*
C*	LUNIN		INTEGER		Data input file unit number	*
C*	FRMT		CHAR*		Model type for data		*
C*	NPRMSF 		INTEGER		Number of surface parms		*
C*	PARMS		CHAR*		Parameter names			*
C*									*
C* Output parameters:							*
C*	STID		CHAR*		Station ID			*
C*	ISTNM		INTEGER		Station number			*
C*	SLAT		REAL		Station latitude (deg)		*
C*	SLON		REAL		Station longitude (deg)		*
C*	SELV		REAL		Station elevation (m)		*
C*	SFDATA (*)	REAL		Array of surface data		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 +1 = End of input file		*
C*					 -8 = Error reading input file	*
C*					-20 = Invalid station list type *
C**									*
C* Log:									*
C* K. Brill/NMC		 2/94						*
C* K. Brill/NMC		 1/95	Add TYPE, convert MRF lat/lon & selv	*
C* V. Krishna Kumar/NCO  5/01   Modified to read the number of          *
C*                              parameters to be plotted. User has to   *
C*                              provide that information.               *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	frmt, parms(*), stid
	REAL		sfdata (npgiv)
C*
	CHARACTER*8	type, cstid
	CHARACTER*1	nrth, west
	SAVE		iseq
	DATA		iseq / 0 /
	DEG ( x ) = FLOAT ( INT (x) ) + FLOAT ( MOD ( INT ( x * 100. +
     +		    .5 ), 100 ) ) / 60.
C----------------------------------------------------------------------
	iret = 0
	cstid = ' '
	CALL ST_LCUC ( frmt, type, ier )
C*
	IF ( type .eq. 'MKLEIN' ) THEN
	    READ ( lunin, *, ERR=10, END=20 ) istnm, stid, slat, slon,
     +		(sfdata (i),i=1,npgiv)
        print*,istnm,stid,slat,slon,(sfdata (i),i=1,npgiv)
C
	    selv = 0.
	    RETURN
	ELSE IF ( type .eq. 'ETAOLD' ) THEN
	    READ ( lunin, 1001, ERR=10, END=20 ) istnm, rlat, nrth,
     +					     rlon, west, selv
1001	    FORMAT (I5,1X,F5.2,A1,1X,F6.2,A1,35X,F4.0)
C*
	ELSE IF ( type .eq. 'ETA' ) THEN
	    READ ( lunin, 1201, ERR=10, END=20 ) istnm, rlat, nrth,
     +					     rlon, west, cstid, ipt,
     +					     iclas, selv
1201	    FORMAT (I6,1X,F5.2,A1,1X,F6.2,A1,1X,A4,1X,2I1,28X,F4.0)
	    sfdata ( 1 ) = istnm
	    sfdata ( 2 ) = ipt
	    sfdata ( 3 ) = iclas
	    stid = cstid
	    IF ( nrth .eq. 'S' ) THEN
		rlat = - rlat
	    END IF
	    IF ( west .eq. 'W' ) THEN
		rlon = - rlon
	    END IF
	    slat = rlat
	    slon = rlon
	    IF ( slon .gt. 180. ) slon = slon - 360.
	    RETURN
	ELSE IF ( type .eq. 'TAF' ) THEN
	    READ (lunin,1004, ERR=10, END=20 ) istnm, slat, slon,
     +		  selv, is, ifl
1004	    FORMAT ( I6,1X,F6.2,1X,F7.2,1X,F5.0,1X,I6,1X,I1 )
	    sfdata (1) = FLOAT (is)
	    sfdata (2) = FLOAT (ifl)
	    stid = ' '
	    RETURN
	ELSE IF ( type .eq. 'MRF' ) THEN
	    READ ( lunin, 1002, ERR=10, END=20 ) istnm, selv, nrth,
     +					rlat, west, rlon
1002	    FORMAT (32X,I5,2X,F4.0,2X,A1,F5.2,1X,A1,F6.2)
C
C*	    Convert deg.min to deg.hundredths.
C
	    rlon = DEG ( rlon )
	    rlat = DEG ( rlat )
C
C*	    Convert feet to meters.
C
	    selv = selv * .3048
C*
	ELSE IF ( type .eq. 'FDWIND' ) THEN
	    READ (lunin,1003,ERR=10,END=20) stid, selv, slat, slon
1003	    FORMAT ( 2X,A3,15X,I5,F6.2,F7.2 )
	    iseq = iseq + 1
	    istnm = iseq
	    sfdata ( 1 ) = istnm
	    slon = - slon
	    RETURN
	ELSE
	    iret = -20
            CALL ER_WMSG  ( 'SFFDSF', iret, 'Invalid list type. ',
     +			    ier )

	    RETURN
	END IF
C*
	sfdata ( 1 ) = istnm
	stid = cstid
	IF ( nrth .eq. 'S' ) THEN
	    rlat = - rlat
	END IF
	IF ( west .eq. 'W' ) THEN
	    rlon = - rlon
	END IF
	slat = rlat
	slon = rlon
C**	print 232, ' STNM, Lat,Lon = ', istnm, slat, slon
C**232	format ( a,i12,', ',f12.4,', ',f12.4)
	IF ( slon .gt. 180. ) slon = slon - 360.
	RETURN
C*
10	CONTINUE
	    iret = -8
	    RETURN
20	CONTINUE
	    iret = 1
	RETURN
	END
