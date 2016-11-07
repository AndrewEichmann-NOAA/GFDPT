	SUBROUTINE SFDSST ( lunsf, stid, istnm, slat, slon, selv,
     +                      dattim, dtmlst, sidlst, isflst, iret )
C************************************************************************
C* SFDSST								*
C*									*
C* This subroutine sets the station and time for both the sounding	*
C* and surface data output files.					*
C*									*
C* SFDSST  ( LUNSF, STID, ISTNM, SLAT, SLON, SELV, DATTIM,		*
C*           DTMLST, SIDLST, ISFLST, IRET )				*
C*									*
C* Input parameters:							*
C*	LUNSF		INTEGER		Sounding file number		*
C*	STID		CHAR*		Station ID			*
C*	ISTNM		INTEGER		Station number			*
C*	SLAT		REAL		Station latitude		*
C*	SLON		REAL		Station longitude		*
C*	SELV		REAL		Station elevation		*
C*	DATTIM		CHAR*		Date/time			*
C*									*
C* Input and output parameters:						*
C*	DTMLST		CHAR*		Previous date/time		*
C*	SIDLST		CHAR*		Previous station ID		*
C*	ISFLST		INTEGER		Previous station number		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* K. Brill/NMC		 9/94						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stid, dattim, dtmlst, sidlst
C*
	CHARACTER	stnid*8, stat*2, coun*2
	DATA		stat, coun / 2 * '  '/
	DATA		ispri /0/
C-----------------------------------------------------------------------
	iret = 0
	IF ( dattim .ne. dtmlst ) THEN
C
C*          If time is not already in files, add time.
C
	    IF ( lunsf .ne. 0 ) THEN
            	CALL SF_FTIM  ( lunsf, dattim, ier )
            	IF  ( ier .ne. 0 )  THEN
                    CALL SF_ATIM  ( lunsf, dattim, ier )
                    IF  ( ier .ne. 0 )  THEN
                    	iret = -16
                    	RETURN
		    ELSE
                	CALL SF_FTIM  ( lunsf, dattim, ier )
                    END IF
            	END IF
	    END IF
	    dtmlst = dattim
	END IF
C*
 	IF ( isflst .ne. istnm ) THEN
C
C*	    If station is not already in files, add it.
C
	    CALL ST_INCH ( istnm, stnid, ier )
	    IF ( lunsf .ne. 0 ) THEN
            	CALL SF_FSTN  ( lunsf, stnid, ier )
            	IF  ( ier .ne. 0 )  THEN
                    CALL SF_ASTN  ( lunsf, 1, stid, istnm, slat,
     +                              slon, selv, stat, coun,
     +				    ispri, nadd, ier )
                    IF  ( ier .ne. 0 )  THEN
		    	iret = -18
		    	RETURN
                    ELSE
                    	CALL SF_FSTN  ( lunsf, stnid, ier )
                    END IF
            	END IF
	    END IF
	    sidlst = stid
	    isflst = istnm
	END IF
C*
	RETURN
	END
