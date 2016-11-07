	SUBROUTINE SFDCHK ( slat, slon, rltln, ok, iret )
C************************************************************************
C* SFDCHK								*
C*									*
C* This subroutine checks to see if a lat/lon point is inside given	*
C* lat/lon bounds.							*
C*									*
C*  SFDCHK ( SLAT, SLON, RLTLN, OK, IRET )				*
C*									*
C* Input parameters:							*
C*	SLAT		REAL		Latitude of point to test	*
C*	SLON		REAL		Longitude of point to test	*
C*	RLTLN (4)	REAL		Lat/lon bounds			*
C*									*
C* Output parameters:							*
C*	OK		LOGICAL		Flag (true for point in bounds)	*
C*	IRET		INTEGER		RETURN code			*
C*									*
C**									*
C* Log:									*
C* K. F. Brill/NMC	 9/94						*
C************************************************************************
	REAL		rltln (4)
	LOGICAL		ok
C------------------------------------------------------------------------
	iret = 0
C*
	rlatll = rltln (1)
	rlonll = rltln (2)
	rlatur = rltln (3)
	rlonur = rltln (4)
	tlon = slon
C*
	IF ( rlonll .gt. rlonur ) THEN
	    rlonll = rlonll - 360.
	    IF ( slon .gt. 0 ) tlon = tlon - 360.
	END IF
	ok = ( slat .ge. rlatll .and. slat .le. rlatur .and.
     +	       tlon .ge. rlonll .and. tlon .le. rlonur )
C*
	RETURN
	END
