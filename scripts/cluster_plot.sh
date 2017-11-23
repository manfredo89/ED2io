#
#PBS -N $mykey
#PBS -q default
#PBS -l walltime=10:00:00
#PBS -l nodes=1:ppn=1
#PBS -m a
#
set -x

ulimit -s unlimited

#module swap cluster/raichu
module load rhdf5/2.18.0-intel-2016b-R-3.3.1

ORIGDIR=$VSC_SCRATCH_VO/manfredo
WORKDIR=$VSC_SCRATCH_NODE/$PBS_JOBID

echo Hostname: $(hostname)
echo ORIGDIR: $ORIGDIR
echo WORKDIR: $WORKDIR

# create working directory and move in there
mkdir -p $WORKDIR
cd $WORKDIR

# copy the R scripts foder
cp -a $ORIGDIR/R $WORKDIR
cp -a $ORIGDIR/utilsR $WORKDIR

 echo -e "Starting the analysis of $mykey \n"

   if [ ! -d "$ORIGDIR/$mykey/analy" ]; then
       echo -e "folder $mykey/analy does not exist \n"
       exit 1

   elif [ ! "$(ls -A $ORIGDIR/$mykey/analy)" ]; then
       echo -e "folder $mykey/analy is empty  \n"
       exit 1

   elif [ $(ls $ORIGDIR/$mykey/analy/*Q* | wc -l) -le 60 ]; then
        echo -e "folder $mykey/analy has probably less than 10 years of simulations  \n"
        exit 1

   else
        echo "folder $mykey is ok"
   fi

#############################################
# copy the analysis and rdata_month folders #
#############################################
   # create and move to general folder
   mkdir -p $mykey
   cd $mykey
   # create figures folder
   mkdir -p figures
   mkdir -p analy
   mkdir -p rdata_month
   # copy stuff I need if present
   rsync -avum $ORIGDIR/$mykey/analy/paracou-Q* analy/
   rsync -avum $ORIGDIR/$mykey/rdata_month/* rdata_month/
   cd ..

#sleep 600

#############################################
#    run the R scripts with my scripts      #
#############################################
cd $WORKDIR/R

      echo "Running my plot script on $mykey"
      Rscript plot.all.manfredo.R $mykey
      
      rsync -avum $WORKDIR/$mykey/figures/* $ORIGDIR/$mykey/figures/
      rsync -avum $WORKDIR/$mykey/rdata_month/* $ORIGDIR/$mykey/rdata_month/

#############################################
#  run the R scripts with Marcos scripts    #
#############################################
cd $WORKDIR/utilsR

      echo "Running Marcos plot script on $mykey"
      Rscript plot_yearly.r $mykey

      rsync -avum $WORKDIR/$mykey/figures/* $ORIGDIR/$mykey/figures/
      rsync -avum $WORKDIR/$mykey/rdata_month/* $ORIGDIR/$mykey/rdata_month/

#cd $WORKDIR

#############################################
#         copy back the figures             #
#############################################
#
#echo "Copying back the results"
#
#   rsync -avum $mykey/figures/* $ORIGDIR/$mykey/figures/
#   rsync -avum $mykey/rdata_month/* $ORIGDIR/$mykey/rdata_month/

# cleanup
rm -Rf $WORKDIR

