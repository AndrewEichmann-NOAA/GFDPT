#!/bin/sh 

#ssmisBUFR_Converter.exe < config/ssmisu_gdas_2013060700_f18.config
ssmisBUFR_Converter.exe < config/ssmisu_gdas_2013060706_f18.config
#ssmisBUFR_Converter.exe < config/ssmisu_gdas_2013060712_f18.config
#ssmisBUFR_Converter.exe < config/ssmisu_gdas_2013060718_f18.config

exit 

# get gfs data
# cd /data/data086/emaddy/dat/dat_old140428/preproc/
# run_ssmis_20130607.sh 

# collocated NWP rad 
exe=/data/data086/emaddy/dat/observation_assessment_tool/bin/colocNWPwRad
#${exe} < config/colocNWP_ecmwf_2013060700_f18.config
${exe} < config/colocNWP_ecmwf_2013060706_f18.config
#${exe} < config/colocNWP_ecmwf_2013060712_f18.config
#${exe} < config/colocNWP_ecmwf_2013060718_f18.config

# FWD 
exe=/data/data086/emaddy/dat/observation_assessment_tool/bin/fwd
#${exe} < config/fwd_ecmwf_2013060700_f18.config
${exe} < config/fwd_ecmwf_2013060706_f18.config
#${exe} < config/fwd_ecmwf_2013060712_f18.config
#${exe} < config/fwd_ecmwf_2013060718_f18.config

#${exe} < config/fwd_gdas_2013060700_f18.config
#${exe} < config/fwd_gdas_2013060706_f18.config
#${exe} < config/fwd_gdas_2013060712_f18.config
#${exe} < config/fwd_gdas_2013060718_f18.config

exit 


${exe} < config/colocNWP_gdas_2013060700_f18.config
${exe} < config/colocNWP_gdas_2013060706_f18.config
${exe} < config/colocNWP_gdas_2013060712_f18.config
${exe} < config/colocNWP_gdas_2013060718_f18.config

# FWD 
exe=/data/data086/emaddy/dat/observation_assessment_tool/bin/fwd


