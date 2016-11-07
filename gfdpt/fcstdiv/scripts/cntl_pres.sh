#!/bin/sh
#set -x

## total field on isobaric layers (SL1L2)

if [ $# -lt 5 ] ; then
 echo "Usage : $0 exp VFDAY HH obtype gd"
 exit
fi

exp=`echo $1 |tr "[a-z]" "[A-Z]" `
VFDAY=$2
HH=$3
obtype=`echo $4 |tr "[a-z]" "[A-Z]" `
gd=`echo $5 |tr "[a-z]" "[A-Z]" `

cat > pres_$1.ctl << EOF
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
    2  SL1L2
       VL1L2
    5  HGT            7  100  0
       T             11 100  0
       U             33 100  0
       V             34 100  0
       WIND          32 100  0
   14  P1000 
       P925 
       P850 
       P700 
       P500 
       P400 
       P300 
       P250 
       P200 
       P150 
       P100 
       P50 
       P20 
       P10
EOF

exit
