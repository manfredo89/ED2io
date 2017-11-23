#!/bin/bash
echo ""
echo "####################################################"
echo "##   This is a script to run the plotting tools   ##"
echo "####################################################"
echo ""

module load rhdf5 
ulimit -s unlimited

DRY=false
MPLOT=false
while getopts ":nm" opt; do
  case $opt in
    n)
      echo "Dry run (-n) was triggered!" >&2
      DRY=true
      ;;
    m)
      echo "I will also perform Marcos scripts." >&2
      MPLOT=true
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
  esac
done
shift $((OPTIND-1))

if [[ $# -eq 0 ]]; then
 echo -e "No arguments folder name provided, defaulting to DIRS.txt\n"
 readarray -t DIRS < DIRS.txt
else
 DIRS=( "$@" )
fi

for simtype in ${DIRS[@]}
do

  echo "Starting $simtype graphs"

  if [ "$DRY" = false ]; then

   echo "Running my script."
   cd R
   (nohup Rscript plot.all.manfredo.R $simtype &> reports/R_${simtype}.out&) &
   cd ..

    #############################################
    #  run the R scripts with Marcos scripts    #
    #############################################
    if [ "$MPLOT" = true ]; then
      echo "Running Marcos script"
      cd utilsR
      (nohup Rscript plot_yearly.r $simtype &> reports/MR_${simtype}.out&) &
      cd ..
    fi
  fi
done

echo "Done!"
exit 0


