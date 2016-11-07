#!/bin/sh

set -xa

PDY=`date "+%Y%m%d"`

devccs=`cat /etc/dev/`
prodccs=`cat /etc/prod`

scp /nco/pmb/f-f-divergence/grib_files/ecm/pgbf*.$PDY* wx12ss@$prodccs:/nco/pmb/f-f-divergence/grib_files/ecm/.

scp /nco/pmb/f-f-divergence/grib_files/pra/pgbf*.$PDY* wx12ss@$prodccs:/nco/pmb/f-f-divergence/grib_files/pra/.

scp /nco/pmb/f-f-divergence/vsdb_data/anom/00Z/pra/pra_$PDY.vsdb wx12ss@$prodccs:/nco/pmb/f-f-divergence/vsdb_data/anom/00Z/pra/.

scp /nco/pmb/f-f-divergence/vsdb_data/anom/12Z/pra/pra_$PDY.vsdb wx12ss@$prodccs:/nco/pmb/f-f-divergence/vsdb_data/anom/12Z/pra/.
