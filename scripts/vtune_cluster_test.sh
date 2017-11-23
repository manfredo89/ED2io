#
#PBS -N Vtune_test
#PBS -q default
#PBS -l walltime=00:10:00
#PBS -l nodes=1:ppn=24
#PBS -e Vtune_cleaned/reports/report.err
#PBS -o Vtune_cleaned/reports/report.out
#PBS -m ae
#
set -x

ulimit -s unlimited

module load HDF5/1.8.17-intel-2016b
module load VTune

ORIGDIR=$PBS_O_WORKDIR
VSCDIR=$VSC_SCRATCH
WORKDIR=$VSC_SCRATCH_NODE/$PBS_JOBID
DESTFOLDER=$ORIGDIR/Vtune_cleaned
echo -e "\n This is a $site simulation \n"
echo Hostname: $(hostname)
echo ORIGDIR: $ORIGDIR
echo WORKDIR: $WORKDIR

mkdir -p $WORKDIR
cd $WORKDIR

copy_data() {

    cp -a $WORKDIR/$analy/* $DESTFOLDER/analy/
    cp -a $WORKDIR/$histo/* $DESTFOLDER/histo/
    cp -a *.txt $DESTFOLDER/reports/
    cp -a r*ah $DESTFOLDER/
}

mkdir -p analy
mkdir -p histo

cp -pfRL $VSCDIR/ED2_data/{FAO,OGE2,Paracou,thermal_sums,csspss} .
cp -pfRL $DESTFOLDER/{ED2IN,ed_2.1-opt_golettF} .
cp $ORIGDIR/cleaned/histo/paracou-S-2300-01-01-000000-g01.h5 ./histo/

amplxe-cl -collect advanced-hotspots -knob event-mode=os -knob analyze-openmp=true ./ed_2.1-opt_golettF

# copy back final output
copy_data

#sleep 600
# cleanup
rm -Rf $WORKDIR
