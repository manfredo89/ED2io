#
#PBS -N $mykey
#PBS -l walltime=48:00:00
#PBS -l nodes=1:ppn=24
#PBS -m ae
#
#set -x

ulimit -s unlimited

#module load ED2/20170201-intel-2017a
module load HDF5

site=paracou
ORIGDIR=$PBS_O_WORKDIR
VSCDIR=$VSC_SCRATCH
WORKDIR=$VSC_SCRATCH_NODE/$PBS_JOBID
DESTFOLDER=$VSC_SCRATCH_VO/manfredo/$mykey
histo=$WORKDIR/histo
analy=$WORKDIR/analy
echo -e "\n This is a $site simulation \n"
echo Hostname: $(hostname)
echo ORIGDIR: $ORIGDIR
echo WORKDIR: $WORKDIR
echo DESTFOLDER: $DESTFOLDER
echo histo: $histo
echo analy: $analy

mkdir -p $WORKDIR
cd $WORKDIR

#functions to check if files/dir exist
fexists()   { [[ -f $1 ]]; }
dexists()   { [[ -d $1 ]]; }
not_empty() { [[ -s $1 ]]; }
# function to copy back partial/final output

plot_data() {

   #module swap cluster/raichu
    qsub -e $DESTFOLDER/reports/Rcluster.err -o $DESTFOLDER/reports/Rcluster.out -N R_$mykey -v mykey=$mykey -l mem=16GB $ORIGDIR/cluster_plot.sh

}

copy_data() {

    if ! dexists $DESTFOLDER; then
        mkdir $DESTFOLDER
        mkdir $DESTFOLDER/analy
        mkdir $DESTFOLDER/histo
    fi

    echo "WRITING ANALY FILES"
    echo "$(ls -l $analy/*)" >> analy.txt
    echo ""
    echo "WRITING HISTO FILES"
    echo "$(ls -l $histo/*)" >> histo.txt

    echo "copying back output from $WORKDIR to $DESTFOLDER"
    rsync --remove-source-files -avum $analy/* $DESTFOLDER/analy/
    rsync -avum $histo/* $DESTFOLDER/histo/

    cp -a *.txt *.dat $DESTFOLDER/reports/

    plot_data
}

# function to sleep for 71h50m and then copy back results

sleep_and_copy() {
    #copy approximately once a day
    while true; do
        sleep 30000s
        echo "I am copying back the data"
        copy_data &
    done
}

update_ED2IN(){

    if fexists ${site}.xml; then

        sed -i -e "s|IEDCNFGF   =.*|IEDCNFGF   = '${site}.xml'|g" ED2IN

    fi

    if fexists $histo/$site-S-*h5; then

        local lastday=`ls -l $histo/$site-S-*h5 | tail -1 | rev | cut -c15-16 | rev`
        local lastmonth=`ls -l $histo/$site-S-*h5 | tail -1 | rev | cut -c18-19 | rev`
        local lastyear=`ls -l $histo/$site-S-*h5 | tail -1 | rev | cut -c21-24 | rev`

        echo $lastday
        echo $lastmonth
        echo $lastyear

        #edit ED2IN history starting point
        sed -i -e "s/IYEARH   =.*/IYEARH   = ${lastyear}   ! Year/" ED2IN
        sed -i -e "s/IMONTHH  =.*/IMONTHH  = ${lastmonth}     ! Month/" ED2IN
        sed -i -e "s/IDATEH   =.*/IDATEH   = ${lastday}     ! Day/" ED2IN

        #edit ED2IN params for history restart
        sed -i -e "s/RUNTYPE  =.*/RUNTYPE  = 'HISTORY'/" ED2IN
        sed -i -e "s|FILIN   =.*|FILIN   = './histo/$site'|g" ED2IN
    fi
}

#copy stuff to work directory (to upgrade copying only strictly needed stuff)
cp -pRL $VSCDIR/ED2_data/{FAO,OGE2,Paracou,thermal_sums,csspss} $WORKDIR/
#cp /apps/gent/CO7/sandybridge/software/ED2/20170201-intel-2017a/bin/ed_2.1-opt $WORKDIR/
cp -fpRL $ORIGDIR/$mykey/{ED2IN,ed_2.1-opt_golett,*.xml} $WORKDIR/

mkdir -p analy
mkdir -p histo

if [ "$(ls -A $ORIGDIR/$mykey/histo)" ]; then
    lastday=`ls -l $ORIGDIR/$mykey/histo/$site-S-*h5 | tail -1 | rev | cut -c15-16 | rev`
    lastmonth=`ls -l $ORIGDIR/$mykey/histo/$site-S-*h5 | tail -1 | rev | cut -c18-19 | rev`
    lastyear=`ls -l $ORIGDIR/$mykey/histo/$site-S-*h5 | tail -1 | rev | cut -c21-24 | rev`

    if not_empty $ORIGDIR/$mykey/histo/$site-S-$lastyear-$lastmonth-$lastday-000000-g01.h5; then
        cp -a $ORIGDIR/$mykey/histo/$site-S-$lastyear-$lastmonth-$lastday-000000-g01.h5 $WORKDIR/histo/
        echo " copied $site-S-$lastyear-$lastmonth-$lastday-000000-g01.h5 "
    else
        echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo "!!!!!!!!!!!!!!!!!!   WARNING   !!!!!!!!!!!!!!!!!!!!!"
        echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo ""
        echo "$ORIGDIR/$mykey/histo/$site-S-$lastyear-$lastmonth-$lastday-000000-g01.h5"
        echo " is broken, check sims and eseg.sh "
        echo ""
        echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    fi
else "No history file found in $ORIGDIR/$mykey/histo "
fi

# I don't like the report to be appended, delete the old ones.
rm $ORIGDIR/${mykey}/reports/${mykey}.{err,out}

# start sleep_and_copy function in the background (done with '&'), remember process ID
sleep_and_copy &
SLEEP_PID=$!

update_ED2IN

#sleep 600
./ed_2.1-opt_golett > ed_output.dat

# kill sleep_and_copy if main program is done
kill -9 $SLEEP_PID

# copy back final output
copy_data

# cleanup
rm -Rf $WORKDIR
