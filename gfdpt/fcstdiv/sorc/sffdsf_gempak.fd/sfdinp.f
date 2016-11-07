	SUBROUTINE  SFDINP  ( sfefil, sfoutf, area, sfprmf, dattim,
     +			      timstn, frmt, iret )
C************************************************************************
C* SFDINP								*
C*									*
C* This subroutine gets the input variables for SFFDSF.			*
C*									*
C* SFDINP  ( SFEFIL, SFOUTF, AREA, SFPRMF, DATTIM, TIMSTN, FRMT, IRET )	*
C**									*
C* Log:									*
C* K. F. Brill/NMC	 9/94						*
C* K. Brill/NMC		 1/95	Add SOURCE				*
C************************************************************************
	CHARACTER*(*)	sfefil, sfoutf, area, sfprmf, dattim,
     +			timstn, frmt
C------------------------------------------------------------------------
C*	Get the input variables.
C
	CALL IP_STR  ( 'SFEFIL', sfefil, ier1 )
	CALL IP_STR  ( 'SFOUTF', sfoutf, ier2 )
	CALL IP_STR  ( 'AREA',   area,   ier3 )
	CALL IP_STR  ( 'SFPRMF', sfprmf, ier4 )
	CALL IP_STR  ( 'DATTIM', dattim, ier5 )
	CALL IP_STR  ( 'TIMSTN', timstn, ier6 )
	CALL IP_STR  ( 'FORMAT',  frmt,   ier7 )
C
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
