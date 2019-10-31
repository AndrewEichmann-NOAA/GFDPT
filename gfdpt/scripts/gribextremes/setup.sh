#!/bin/bash
#
#  script:  setup  Author:  Bill Facey  4/3/96
#
# Abstract:  This script copies various script utiities to the
# working directory so that they may be used in our operational
# scripts.  The following utility scripts are setup: tracer,
# prep_step.sh, err_chk.sh, err_exit.sh, null, and postmsg.sh.
# Before using these scripts, review them.
# NOTE - The following can be  used interchangeably;
#
# USAGE:  Script assumes you are in the working directory when
# you use it.
# ###############################################
# NOW SETUP -HERE FILES- ROUTINES FOR PROCESSING
# ###############################################
USHutil=${USHutil:-/nwprod/util/ush}

##################################################################
# UTILITY - script to initialize files before each step/execution
##################################################################
diff $USHutil/prep_step.sh prep_step 2>/dev/null
if [ $? -ne 0 ] ; then
  cp $USHutil/prep_step.sh prep_step
fi
chmod u+x prep_step
 
#####################################
# UTILITY - setup error-check script
#####################################
diff $USHutil/err_chk.sh err_chk 2>/dev/null
if [ $? -ne 0 ] ; then
  cp $USHutil/err_chk.sh err_chk
# errchk is DEPRECATED. Please use err_chk
  cp $USHutil/err_chk.sh errchk
fi
chmod u+x err_chk errchk

####################################
# UTILITY - setup error-exit script
####################################
diff $USHutil/errexit.sh err_exit 2>/dev/null
if [ $? -ne 0 ] ; then
  cp $USHutil/errexit.sh err_exit
# errexit is DEPRECATED. Please use err_exit
  cp $USHutil/errexit.sh errexit
fi
chmod u+x err_exit errexit

################################
# UTILITY - text for page break
################################
rm break 2> /dev/null
tr="step ############# break ##############################"
echo "$tr">  break
chmod u+x break
 
############################################
# UTILITY - text to establish a null command
#############################################
rm null 2> /dev/null
echo " " > null
chmod u+x null
 
#############################################
# UTILITY - text to establish postmsg command
#############################################
diff $USHutil/postmsg.sh postmsg  2>/dev/null
if [ $? -ne 0 ] ; then
  cp $USHutil/postmsg.sh postmsg
fi
chmod u+x postmsg
 
#############################################
# UTILITY - text to establish startmsg command
#############################################
diff $USHutil/startmsg.sh startmsg  2>/dev/null
if [ $? -ne 0 ] ; then
  cp $USHutil/startmsg.sh startmsg
fi
chmod u+x startmsg

#############################################
# UTILITY - require that a copy be successful 
#############################################
cp $USHutil/cpreq.sh cpreq
chmod u+x cpreq

#############################################
# UTILITY - copy across file systems and require that the copy be successful 
#############################################
cp $USHutil/cpfs.sh cpfs
chmod u+x cpfs

#  --------------------end setup script-----------------------------
