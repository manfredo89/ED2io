#!/bin/bash
echo ""
echo "####################################################"
echo "##   This is a script to restart ED simulations   ##"
echo "####################################################"
echo ""

LAUNCH=launch
while getopts ":n" opt; do
  case $opt in
    n)
      echo "Dry run (-n) was triggered!" >&2
      LAUNCH=nolaunch
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
  esac
done

which_cluster=$(module list | grep cluster | sed 's/.*cluster\///' | cut -c1-6)
if [[ $which_cluster != "raichu" && $which_cluster != "delcat" && $which_cluster != "golett" ]]; then
   echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
   echo "!!!!!!!!!!!!!!!!     ERROR     !!!!!!!!!!!!!!!!!!!!!"
   echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
   echo ""
   echo "Cluster is neither raic/del nor golett, I have not  "
   echo "tested the script outside of Ugent cluster. If you  "
   echo "are sure about what you are doing uncheck this IF.  "
   echo ""
   echo "For now I am leaving, BYE!"
   
   exit 1

elif [[ $which_cluster == "delcat" ]]; then
   echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
   echo "!!!!!!!!!!!!!!!!    WARNING    !!!!!!!!!!!!!!!!!!!!!"
   echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
   echo ""
   read -r -p "Cluster is DELCATTY, are you sure you want to continue? [y/N]" response
   response=${response,,}    # tolower

if [[ $response =~ ^(no|n)$ ]]; then
   exit 1
fi
fi

unset which_cluster

# This is the list of simulations that you want
# to restart

if [[ $# -eq 0 ]]; then
   echo -e "No arguments folder name provided, defaulting to DIRS.txt\n"
   readarray -t DIRS < DIRS.txt
else
   DIRS=("$@")
   DIRS=${DIRS%/}
fi

# get the job identifier and store it in an array
JOBS=($(qstat -n | tail -n+6 | awk '{print$4}'))

# for the directory trim the string so as to have it
# in the same length of JOBS
for val2 in "${JOBS[@]}"; do
    for val1 in "${DIRS[@]}"; do
        if [[ ${val1:0:16} == "${val2:0:16}" ]]; then
            JOBSl+=("$val1")
            break
        fi
    done
done

# print the two arrays for the user to visually compare
printf "DIRS                \tJOBS\n"
paste <(printf "%-20.20s\n" "${DIRS[@]}"|sort) \
      <(printf "%-20.20s\n" "${JOBS[@]}"|sort)

# mysim is the result of the comparison between
# running jobs and DIRS, if an element of DIRS
# is not among jobs I will attempt to restart it
mysim="$(echo ${DIRS[@]} ${JOBSl[@]} | tr ' ' '\n' | sort | uniq -u)"
unset JOBSl

# Restart
if [[ ${mysim[@]:+${mysim[@]}} ]]; then
   echo -e "\nI have found some DIRS that have no corresponding jobs"
   echo -e "I will now try to restart these jobs:\n"
   echo -e "$( echo ${mysim[@]} | tr ' ' '\n' | sort | uniq -u )\n"
   
   for i in ${mysim[@]}; do
   if [[ $LAUNCH == "launch" ]]; then
      echo -e "\nRestarting $i simulation"
      qsub -e $i/reports/$i.err -o $i/reports/$i.out -N $i -v mykey=$i eseg.sh       
   fi
   done
   qstat -n
   exit 0
else
   echo -e "All simulations are up and running, BYE\n"
   qstat -n
   exit 0
fi



