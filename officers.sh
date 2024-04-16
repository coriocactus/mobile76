#!/bin/bash

fdate=$(date '+%y%m%d')

echo cid,pid,doa,fname,lname,hon,cof,dob,title,po,ad1,ad2,town,cnty,ctry,pc > $fdate.csv

if [[ "${TEST:-0}" -gt 0 ]]; then
  sed '/^$/d' officer_snapshot.txt | python3 officers.py >> $fdate.csv
  cat $fdate-*.log > $fdate.log
  rm $fdate-*.log
else
  sed '/^$/d' ${1:-officer_snapshot.txt} | parallel --pipe -N1000000 --joblog $fdate.parallel python3 officers.py >> $fdate.csv
  cat $fdate-*.log > $fdate.log
  rm $fdate-*.log
fi

