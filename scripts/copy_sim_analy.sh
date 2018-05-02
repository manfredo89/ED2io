#!/bin/bash
# This script retrieves analysis file from ED2 simulations 
# taht are currently running on cluster nodes.

DRY=false
HISTORY=true
while getopts ":ns" opt; do
  case $opt in
    n)
      echo "Dry run (-n) was triggered!" >&2
      DRY=true
      ;;
    s)
      echo "I will not copy history and txt files." >&2
      HISTORY=false
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
  esac
done
shift $((OPTIND-1))


#DIRS=( lianas        \
#nolianas             \
#inventory_lianas     \
#inventory_nolianas   )

if [[ $# -eq 0 ]]; then
   echo -e "No arguments folder name provided, defaulting to DIRS.txt\n"
   readarray -t DIRS < DIRS.txt
else
   DIRS=("$@")
   DIRS=${DIRS%/}
fi

for simtype in ${DIRS[@]}
do

  echo -e "I am about to ssh to $simtype on worker node\n"
  if [ "$DRY" = false ]; then

   MNAM=$(qselect -s R -N $simtype | cut -c1-18 | head -1)
   MYNODE=$(qstat -n | grep -A1 "$MNAM" | grep "node")
   MYCL=$(qselect -s R -N $simtype | cut -f3,4,5 -d'.' | head -1)
   echo -e "node    ---> $MYNODE\n"
   echo -e "cluster ---> $MYCL  \n"
    if [ "$HISTORY" = true ]; then

      ssh ${MYNODE}.${MYCL} "rsync --remove-source-files -avum /local/$(qselect -s R -N $simtype | head -1)/analy/* $VSC_SCRATCH_VO/manfredo/$simtype/analy/;"\
     "rsync -avum /local/$(qselect -s R -N $simtype | head -1)/histo/* $VSC_SCRATCH_VO/manfredo/$simtype/histo/;"\
     "rsync -avum /local/$(qselect -s R -N $simtype | head -1)/allom_param.txt $VSC_SCRATCH_VO/manfredo/$simtype/;"\
     "rsync -avum /local/$(qselect -s R -N $simtype | head -1)/ed_output.dat $VSC_SCRATCH_VO/manfredo/$simtype/reports/"
    else
      "echo "$(ls -l $analy/*)" >> analy.txt;"\
      "cp analy.txt $VSC_SCRATCH_VO/manfredo/$simtype/reports/;"\
      "cp ed_output.dat $VSC_SCRATCH_VO/manfredo/$simtype/reports/;"\
      ssh ${MYNODE}.${MYCL} "rsync --remove-source-files -avum /local/$(qselect -s R -N $simtype | head -1)/analy/* $VSC_SCRATCH_VO/manfredo/$simtype/analy/;"
    fi
  else
    echo "Trying to sync $simtype"
  fi
done


