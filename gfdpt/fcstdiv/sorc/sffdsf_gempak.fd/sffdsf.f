	PROGRAM SFFDSF
C************************************************************************
C* PROGRAM SFFDSF							*
C*									*
C* This programs reads text formatted surface data and writes it to	*
C* a GEMPAK surface dataset.						*
C* Modified to read the number of parameters to be plotted.             * 
C*									*
C* The parameter packing files must be provided by the user.		*
C**									*
C* Log:									*
C* K. Brill/NMC		 9/94						*
C* V. Krishna Kumar/NCO  5/01
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	sfefil*72, sfoutf*72, sfprmf*72, dattim*72,
     +			area*72, timstn*32, frmt*12
	CHARACTER	dtmlst*20, stid*8, sidlst*8
	CHARACTER*4	parmsf (MMPARM), cprj
	REAL		sfdata (MMPARM), rltln (4), centrd (2)
	LOGICAL		respnd, done, proces, ok
	DATA		ifhr /0/
        DATA            LULST /9/
C
C* Reads the number of parameters to be plotted from the namelist
C  Check the appropriate scripts 
C
        NAMELIST /INPARM/ npgiv
        READ(LULST,INPARM)
C------------------------------------------------------------------------
C*	Initialize TAE.
C
	CALL IP_INIT  ( respnd, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -1
	    CALL ER_WMSG  ( 'SFFDSF', iret, ' ', ier )
	    CALL SS_EXIT
	END IF
	CALL IP_IDNT  ( 'SFFDSF', ier )
	done = .false.
C
C*	Loop through program until user exits.
C
	DO WHILE  ( .not. done )
C
C*	    Get input.
C
	    CALL SFDINP  ( sfefil, sfoutf, area, sfprmf, dattim,
     +			   timstn, frmt, iret )
	    IF  ( iret .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SFFDSF', iret, ' ', ier )
		CALL SS_EXIT
	    END IF
	    proces = .true.
C
C*	    Open the surface data file, process the parameter
C*	    files and open the output files.  Also read in the
C*	    conversion factors for the parameters.
C
	    CALL SFDOPN ( sfefil, sfoutf, sfprmf, 
     +			  timstn, lunin, lunsf, nprmsf,
     +			  parmsf, iret )
	    IF  ( iret .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SFFDSF', iret, sfefil, ier )
		proces = .false.
	    END IF
C
C*	    Set the subset area.
C
	    CALL LC_GARE ( area, rltln, cprj, centrd, ier )
C
C*	    Read the input file; write to the output file.
C
	    dtmlst = ' '
	    sidlst = ' '
	    isflst = 0
	    icnt = 0
C
	    DO WHILE ( proces )
		CALL SFDRDD ( lunin, frmt, nprmsf, parmsf,
     +			      npgiv, stid, istnm, slat, slon,
     +			      selv, sfdata, iret )
		CALL SFDCHK ( slat, slon, rltln, ok, ier )
		IF ( iret .ne. 0 ) THEN
		    proces = .false.
		    CALL ER_WMSG ( 'SFFDSF', iret, ' ', ier )
		ELSE IF ( ok ) THEN
C
C*		    Set station and time for writing.
C
		    CALL SFDSST ( lunsf, stid, istnm,
     +			          slat, slon, selv, dattim,
     +				  dtmlst, sidlst, isflst, ierx )
		    IF ( ierx .eq. 0 .and. lunsf .ne. 0 ) THEN
     			CALL SF_WDAT ( lunsf, ifhr, sfdata,
     +     			       ier )
			IF ( ier .ne. 0 ) THEN
			    CALL ER_WMSG ( 'SF', ier, ' ', ierr )
			ELSE
			    icnt = icnt + 1
			END IF
		    ELSE
			CALL ER_WMSG ( 'SFFDSF', ierx, ' ', ier )
		    END IF
		END IF
	    END DO
	    WRITE (6,*) icnt, ' stations found.'
	    CALL FL_CLOS  ( lunin, ier )
	    CALL SF_CLOS  ( lunsf, ier )
C
C*	    Call dynamic tutor.
C
	    CALL IP_DYNM  ( done, ier )
	END DO
C
C*	Exit.
C
	CALL IP_EXIT  ( ier )
C*
	END		
