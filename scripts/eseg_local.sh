#
#PBS -N $mykey
#PBS -l walltime=24:00:00
#PBS -l nodes=1:ppn=24
#PBS -m ae
#
#set -x

ulimit -s unlimited

module load HDF5

site=paracou
DESTFOLDER=$VSC_SCRATCH_VO/manfredo/$mykey
ORIGDIR=$PBS_O_WORKDIR

#functions to check if files/dir exist
fexists()   { [[ -f $1 ]]; }
dexists()   { [[ -d $1 ]]; }
not_empty() { [[ -s $1 ]]; }


plot_data() {

    qsub -e $DESTFOLDER/reports/Rcluster.err -o $DESTFOLDER/reports/Rcluster.out -N R_$mykey -v mykey=$mykey -l mem=16GB $ORIGDIR/cluster_plot.sh

}

update_ED2IN(){

    cp ED2IN ED2IN_sim
    if fexists ${site}.xml; then

        sed -i -e "s|IEDCNFGF   =.*|IEDCNFGF   = '${site}.xml'|g" ED2IN_sim

    fi

    if fexists histo/$site-S-*h5; then

        local lastday=`ls -l histo/$site-S-*h5 | tail -1 | rev | cut -c15-16 | rev`
        local lastmonth=`ls -l histo/$site-S-*h5 | tail -1 | rev | cut -c18-19 | rev`
        local lastyear=`ls -l histo/$site-S-*h5 | tail -1 | rev | cut -c21-24 | rev`

        echo $lastday
        echo $lastmonth
        echo $lastyear

        #edit ED2IN history starting point
        sed -i -e "s/IYEARH   =.*/IYEARH   = ${lastyear}   ! Year/" ED2IN_sim
        sed -i -e "s/IMONTHH  =.*/IMONTHH  = ${lastmonth}     ! Month/" ED2IN_sim
        sed -i -e "s/IDATEH   =.*/IDATEH   = ${lastday}     ! Day/" ED2IN_sim

        #edit ED2IN params for history restart
        sed -i -e "s/RUNTYPE  =.*/RUNTYPE  = 'HISTORY'/" ED2IN_sim
        sed -i -e "s|FILIN   =.*|FILIN   = './histo/$site'|g" ED2IN_sim
    fi
}


# I don't like the report to be appended, delete the old ones.
rm $ORIGDIR/${mykey}/reports/${mykey}.{err,out}

cd $DESTFOLDER
update_ED2IN

./ed_2.1-opt_golett -f ED2IN_sim > reports/ed_output.dat


