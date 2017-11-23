#!/bin/bash

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

#find . -type f -name "*.err" -exec rm -rf {} \;
#find . -type f -name "*.out" -exec rm -rf {} \;

if [[ $# -eq 0 ]]; then
   echo -e "No arguments folder name provided, defaulting to DIRS.txt\n"

   DIRS=(  r_lianas     \
   r_inventory_lianas   )
else
   DIRS=( "$@" )
fi

for simtype in ${DIRS[@]}
do

echo -e "Deleting $simtype content\n"
if [[ $LAUNCH == "launch" ]]; then

   mkdir -p $simtype
   cd $simtype
   mkdir -p reports analy histo figures rdata_month
   rm -rf reports/* analy/* histo/* figures/* rdata_month/*
   cd ..
fi
done


