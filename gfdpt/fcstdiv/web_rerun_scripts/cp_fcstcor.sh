#!/bin/sh

set -xa

export PDY=`date "+%Y%m%d"`
echo $PDY

mkdir -p /ptmp/wx12ss/fcstdiv/fcst_corr.$PDY

ssh -l sdm ncointra "mkdir -p /home/nco/sdm/sdm/fcstdiv/main/correlations/fcst_corr.$PDY"

scp /stmp/wx12ss/f-f-divergence/vsdb_stats/anom/pra/HGT/cor_*HX.png sdm@ncointra:/home/nco/sdm/sdm/fcstdiv/main/correlations/fcst_corr.$PDY
scp /stmp/wx12ss/f-f-divergence/vsdb_stats/anom/pra/T/cor_*HX.png sdm@ncointra:/home/nco/sdm/sdm/fcstdiv/main/correlations/fcst_corr.$PDY
scp /stmp/wx12ss/f-f-divergence/vsdb_stats/anom/pra/U/cor_*HX.png sdm@ncointra:/home/nco/sdm/sdm/fcstdiv/main/correlations/fcst_corr.$PDY
scp /stmp/wx12ss/f-f-divergence/vsdb_stats/anom/pra/V/cor_*HX.png sdm@ncointra:/home/nco/sdm/sdm/fcstdiv/main/correlations/fcst_corr.$PDY
scp /stmp/wx12ss/f-f-divergence/vsdb_stats/anom/pra/WIND/cor_*HX.png sdm@ncointra:/home/nco/sdm/sdm/fcstdiv/main/correlations/fcst_corr.$PDY
scp /nco/pmb/wx12ss/fcstdiv/fcstcor_convert.sh sdm@ncointra:/home/nco/sdm/sdm/fcstdiv/main/correlations/fcst_corr.$PDY

ssh -l sdm ncointra "sh /home/nco/sdm/sdm/fcstdiv/main/correlations/fcst_corr.$PDY/fcstcor_convert.sh $PDY"
