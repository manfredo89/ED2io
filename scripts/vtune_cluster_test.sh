#
#PBS -N Vtune_test
#PBS -q default
#PBS -l walltime=02:00:00
#PBS -l nodes=1:ppn=24
#PBS -e Vtune_cleaned/reports/report.err
#PBS -o Vtune_cleaned/reports/report.out
#PBS -m ae
#
set -x

ulimit -s unlimited

ml VTune
ml HDF5

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

    #cp -a $WORKDIR/analy/* $DESTFOLDER/analy/
    #cp -a $WORKDIR/histo/* $DESTFOLDER/histo/
    #cp -a *.txt $DESTFOLDER/reports/
    cp -a r* $DESTFOLDER/
}

mkdir -p analy
mkdir -p histo

cp -pfRL $DESTFOLDER/{ED2IN*,ed_2.1-opt_golett} .
cp $ORIGDIR/Vtune_cleaned/histo/* histo/

for version in ED2IN*; do
    mkdir -p r_$version
    echo '#!/bin/bash' > exec.sh
    echo "./ed_2.1-opt_golett -f $version" >> exec.sh
    chmod +x exec.sh
    amplxe-cl -collect advanced-hotspots -result-dir r_$version -knob event-mode=os -knob analyze-openmp=true ./exec.sh
done

# copy back final output
copy_data

# cleanup
rm -Rf $WORKDIR
