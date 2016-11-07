            SUBROUTINE  SFDOPN ( sfefil, sfoutf, sfprmf, timstn,
     +                    	 lunin, lunsf, nprmsf, parmsf, iret )
C************************************************************************
C* SFDOPN								*
C*									*
C* This subroutine opens the files needed by SFDOPN.  The output file	*
C* is opened or created and the number of parameters is determined.	*
C*									*
C* SFDOPN  ( SFEFIL, SFOUTF, SFPRMF, TIMSTN,				*
C*	      LUNIN, LUNSF, NPRMSF, PARMSF, PARMSF, IRET )		*
C*									*
C* Input parameters:							*
C*	SFEFIL		CHAR*		Surface data file name		*
C*	SFOUTF		CHAR*		Output surface file name	*
C*	SFPRMF		CHAR*		Surface parm packing file name 	*
C*	TIMSTN		CHAR*		Maximum times / stations	*
C*									*
C* Output parameters:							*
C*	LUNIN		INTEGER		Input data file number		*
C*	LUNSF		INTEGER		Output sfd data file number	*
C*	NPRMSF		INTEGER		Number of surface parms		*
C*	PARMSF (NPRMSF) CHAR*4		Surface parm list		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -1 = file not open		*
C**									*
C* Log:									*
C* K. Brill/NMC		 2/94						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	sfefil, sfoutf, sfprmf, timstn
	CHARACTER*(*)   parmsf (*)
C*
        INTEGER         iscl (MMPARM), ioff (MMPARM), ibit (MMPARM)
	INTEGER		ilist (2)
        CHARACTER       reqprm (MMPARM)*4, filprm (MMPARM)*4
	LOGICAL		pkflg
C----------------------------------------------------------------------
	iret  = 0
	lunsf = 0
	nprmsf = 0
C
C*	Open the surface output file.
C
	IF ( sfoutf .ne. ' ' ) THEN
	    CALL SF_OPNF ( sfoutf, .true., lunsf, iflsrc, npsf,
     +			   filprm, ier )
	    IF ( ier .eq. 0 ) THEN
	    	CALL DP_FILE ( sfprmf, npreq, reqprm, iscl,
     +			       ioff, ibit, pkflg, ier )
	    	IF ( ier .ne. 0 ) THEN
		    iret = -4
		    RETURN
	    	END IF
	    	IF ( npsf .ne. npreq ) THEN
		    iret = -5
		    RETURN
	    	END IF
	    	DO i = 1, npsf
		    IF ( filprm (i) .ne. reqprm (i) ) THEN
		    	iret = -6
		    	RETURN
		    END IF
	    	END DO
	    ELSE
C
C*	        Create a new surface data file.
C
C*          	Get the number of times and the number of stations to
C*          	add.
C
            	CALL ST_ILST  ( timstn, '/', IMISSD, 2, ilist, n,
     +				ier )
            	IF  ( ilist (1) .eq. IMISSD )  THEN
                    maxtim = 49
            	ELSE
                    maxtim = ilist (1)
            	END IF
            	IF  ( ilist (2) .eq. IMISSD )  THEN
                    maxstn = 700
            	ELSE
                    maxstn = ilist (2)
            	END IF
	    	CALL SF_CRFP ( sfoutf, sfprmf, 0, maxstn, maxtim, 
     +			       .true., lunsf, npsf, filprm, pkflg,
     +			       ier )
	    	IF ( ier .ne. 0 ) THEN
		    iret = -7
		    RETURN
	    	END IF
	    	CALL ER_WMSG ( 'SFMDSF', 2, ' ', ier )
	    END IF
	    nprmsf = npsf
	    DO ip = 1, nprmsf
	        parmsf (ip) = filprm (ip)
	    END DO
	END IF
C
C*	Open the input file.
C
	CALL FL_SOPN ( sfefil, lunin, iostat )
	IF ( iostat .ne. 0 ) THEN
	    iret = -3
	    RETURN
	END IF
C*
	RETURN
	END
