#!/bin/bash
#set -x
if [[ $# -eq 0 ]]; then
   echo "No arguments folder name provided, ABORTING"
   exit 1
else
   DIR=$1
fi

cd $DIR/analy

time1="1520956124"
echo "#CPU time" > ../reports/CPU_times.dat

for file in  $(ls paracou-Q-*.h5); do

	year=$(echo $file | cut -c11-14)
	month=$(echo $file | cut -c16-17)
	month_f=$(echo "$month * 0.08333333" | bc -l)

	my_date=$(echo "$year + $month_f" | bc -l)


	gmt=$(stat -c "%y" $file | cut -c1-19)
	time2=$(date +%s -d "$gmt")

	echo -e "$my_date\t$(($time2-$time1))" >> ../reports/CPU_times.dat

	time1=$time2

done

exit 0
