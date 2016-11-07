#!/bin/sh
#set -x

## anomaly type (SAL1L2 and VAL1L2)

if [ $# -lt 5 ] ; then
 echo "Usage : $0 exp VFDAY HH obtype gd"
 exit
fi

## exp and obtype must be capital letters
exp=`echo $1 |tr "[a-z]" "[A-Z]" `
VFDAY=$2
HH=$3
obtype=`echo $4 |tr "[a-z]" "[A-Z]" `
gd=`echo $5 |tr "[a-z]" "[A-Z]" `

cat > anom_$1.ctl << EOF
V01   10
    1 $exp
    17  0              
       24
       48
       72
       96
       120
       144
       168
       192
       216
       240
       264
       288
       312
       336
       360
       384
    1  ${VFDAY}${HH}  
    1  ${obtype}     
    4  ${gd}           
       ${gd}/NHX 0  20  360  80
       ${gd}/SHX 0 -80  360 -20
       ${gd}/TRO 0 -20  360  20
    2  SAL1L2
       VAL1L2
   10  HGT            7  100  0
       HGT_WV1/0-3    7  100  0
       HGT_WV1/4-9    7  100  0
       HGT_WV1/10-20  7  100  0
       HGT_WV1/0-20   7  100  0
       PMSL          2  102  0
       T             11 100  0
       U             33 100  0
       V             34 100  0
       WIND          32 100  0
   7   P1000
       P850
       P700
       P500
       P250
       P200
       P100
EOF

exit
