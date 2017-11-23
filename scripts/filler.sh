#!/bin/bash
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

#set -x

ulimit -s unlimited

if [ $# -eq 0 ]
then
    echo "Usage is ./filler [-n] <simulation_name>"
    exit 1
fi


echo ""
echo "####################################################"
echo "# This is a script to fill the gaps in the ED sims #"
echo "####################################################"
echo ""

LAUNCH=launch
while getopts ":n" opt; do
    case $opt in
        n)
            echo "Dry run (-n) was triggered!" >&2
            LAUNCH=nolaunch
            shift
            ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            exit 1
            ;;
    esac
done

update_ED2IN_years(){


    sed -i -e "s/IMONTHH  =.*/IMONTHH  = $1     ! Month/" ED2IN
    sed -i -e "s/IYEARH   =.*/IYEARH   = $2   ! Year/" ED2IN

    sed -i -e "s/IMONTHZ  =.*/IMONTHZ  = $3    ! Month/" ED2IN
    sed -i -e "s/IYEARZ   =.*/IYEARZ   = $4  ! Year/" ED2IN
    #edit ED2IN params for history restart
    #next line is used when you spin up the model but we don't
    #sed -i 's/IED_INIT_MODE   =.*/IED_INIT_MODE   = 5/' ED2IN
}

update_ED2IN_paths(){

    local vg="NL%VEG_DATABASE"
    local sd="NL%SOIL_DATABASE"
    local ts="NL%THSUMS_DATABASE"
    local md="NL%ED_MET_DRIVER_DB"
    local path="$VSC_SCRATCH/ED2/ED/build"

    #    sed -i -e "s|$vg      =.*|$vg      = '$path/OGE2/OGE2_'|g" ED2IN
    #    sed -i -e "s|$sd     =.*|$sd     = '$path/FAO/FAO_'|g" ED2IN
    #    sed -i -e "s|$ts   =.*|$ts   = '$path/thermal_sums/'|g" ED2IN
    #    sed -i -e "s|$md  =.*|$md  = '$path/Paracou/Paracou_HEADER'|g" ED2IN
    sed -i -e "s|RUNTYPE  =.*|RUNTYPE  = 'HISTORY'|g" ED2IN
    sed -i -e "s|FILIN   =.*|FILIN   = './histo/paracou'|g" ED2IN
}

#increment_month(){}

#increment_year(){}

if [[ $# -eq 0 ]]; then
    echo -e "No arguments folder name provided, defaulting to DIRS.txt\n"
    readarray -t DIRS < DIRS.txt
else
    DIRS=( "$@" )
fi

for simtype in ${DIRS[@]}
do
    cd $VSC_SCRATCH_VO/manfredo/$simtype

    firstmonth=`ls -l analy/paracou-Q-*h5 | head -1 | rev | cut -c18-19 | rev`
    firstyear=`ls -l analy/paracou-Q-*h5 | head -1 | rev | cut -c21-24 | rev`

    lastmonth=`ls -l analy/paracou-Q-*h5 | tail -1 | rev | cut -c18-19 | rev`
    lastyear=`ls -l analy/paracou-Q-*h5 | tail -1 | rev | cut -c21-24 | rev`

    firstmonth=$((10#$firstmonth))
    lastmonth=$((10#$lastmonth))

    month_count=$(((lastyear - firstyear) * 12 + (lastmonth - firstmonth) + 1))
    file_count=`ls -l analy/paracou-Q-*h5 | wc -l`

    echo "file_count $file_count month_count $month_count"
    if [[ $month_count -eq $file_count ]]
    then
        echo "############################################################################"
        echo -e "${GREEN}All analysis seem to be there. Leaving $simtype directory.${NC}"
        echo "############################################################################"
        exit 0
    else
        echo "############################################################################"
        echo -e "${RED}I found some missing analysis files in $simtype directory.${NC}"
        echo "I will now attempt to run a simulation in background to fill in the gaps."
        echo "############################################################################"

        if [[ $LAUNCH == "launch" ]]; then

            mkdir -p temp
            mkdir -p temp/analy
            mkdir -p temp/histo
            cd temp
            cp $VSC_SCRATCH_VO/manfredo/$simtype/ed_2.1-opt* .	
            #	cp $VSC_SCRATCH/ED_$simtype/ED/build/ed_2.1-opt* .
            cp ../ED2IN .

            update_ED2IN_paths

            i="1"
            while [[ $i -lt $month_count ]]
            do
                #echo "i is ${i}"
                current_month=$(( (i - 1) % 12 + 1 + firsmonth ))
                current_year=$(( i / 12 + firstyear ))

                if [[ ${#current_month} < 2 ]]
                then current_month="0${current_month}"
                fi

                if [[ ! -e ../analy/paracou-Q-${current_year}-${current_month}-00-000000-g01.h5 ]] && [[ $current_year -lt $lastyear ]]
                then

                    echo "$(pwd)"
                    echo "I have not found the file paracou-Q-${current_year}-${current_month}-00-000000-g01.h5"

                    # delta_month is the number of month that I check inside this conditional
                    # I keep track of ot because in the big while loop I can skip these months

                    delta_month=2

                    monthh=$current_month
                    yearh=$current_year

                    # Set monthz/yearz to one month later than monthh/yearh
                    if [ $monthh -eq 12 ]
                    then
                        monthz=1
                        yearz=$((yearh + 1))
                    else
                        monthz=$((10#$monthh + 1))
                        yearz=$yearh
                    fi

                    # Print the correct number of digits
                    if [[ ${#monthz} < 2 ]]
                    then monthz="0${monthz}"
                    fi

                    while [[ ! -e ../analy/paracou-Q-${yearz}-${monthz}-00-000000-g01.h5 ]] && [[ $yearz -lt $lastyear ]]
                    do
                        #          echo "paracou-Q-${yearz}-${monthz}-00-000000-g01.h5"
                        #          echo "yearz is $yearz monthz is $monthz"
                        # Increse the monthz
                        if [ $monthz -eq 12 ]
                        then
                            monthz=1
                            (( yearz++ ))
                        else
                            monthz=$((10#$monthz + 1))
                        fi

                        if [[ ${#monthz} < 2 ]]
                        then monthz="0${monthz}"
                        fi
                        # Update the general index as well
                        (( delta_month++ ))
                    done
                    #echo "allora : $i    $delta_month"
                    i=$(( i + delta_month ))
                    #echo "dopo : $i"
                    #        echo "$monthh $yearh $monthz $yearz"
                    update_ED2IN_years $monthh $yearh $monthz $yearz

                    if [[ ${#monthh} < 2 ]]
                    then monthh="0${monthh}"
                    fi

                    cp ../histo/paracou-S-${yearh}-${monthh}-01-000000-g01.h5 histo/

                    ./ed_2.1-opt*
                    rsync -ahm --ignore-existing analy/* ../analy/
                    echo "Files copied"
                else
                    ((i++))

                fi
            done
            rm -fr ../temp
            exit 0
        else
            exit 1
        fi
    fi
done

#unset i
#unset delta_month

