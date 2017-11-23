#!/bin/bash
#This script is just a wrapper to launch the cluster plot 

if [[ $# -eq 0 ]]; then
   echo -e "No arguments folder name provided, defaulting to DIRS.txt\n"
   readarray -t DIRS < DIRS.txt
else
   DIRS=("$@")
   DIRS=${DIRS%/}
fi

for simtype in ${DIRS[@]}
do
    ./copy_sim_analy.sh $simtype
done

for simtype in ${DIRS[@]}
do
    if ./filler.sh -n $simtype ; then

        echo -e "I am about starting the cluster plot tool for $simtype\n"
        qsub -e $simtype/reports/Rcluster.err -o $simtype/reports/Rcluster.out -N R_$simtype -v mykey=$simtype -l mem=16GB cluster_plot.sh
    else
        echo -e "Filler script reported missing files in folder $simtype: ABORTING!"
    fi

done

qstat

exit 0
