#==========================================================================================#
#==========================================================================================#
#     Create the monthly mean structure to be filled by plot_monthly.r                     #
#------------------------------------------------------------------------------------------#
create.monthly <<- function(ntimes,yeara,inpref){

  #----- Read the first HDF5 to grab some simulation-dependent dimensions. ---------------#
  #----- montha was a function argument, to simplify I set it to 1 here. -----------------#

  montha       = 01
  cyear        = sprintf("%4.4i",yeara )
  cmonth       = sprintf("%2.2i",1     )
  h5first      = paste(inpref,"-Q-",cyear,"-",cmonth,"-00-000000-g01.h5"    ,sep="")
  h5first.bz2  = paste(inpref,"-Q-",cyear,"-",cmonth,"-00-000000-g01.h5.bz2",sep="")
  h5first.gz   = paste(inpref,"-Q-",cyear,"-",cmonth,"-00-000000-g01.h5.gz" ,sep="")
  if ( file.exists(h5first) ){
    mymont    = H5Fopen(h5first)

  }else if ( file.exists(h5first.bz2) ){
    temp.file = file.path(tempdir(),basename(h5first))
    dummy     = bunzip2(filename=h5first.bz2,destname=temp.file,remove=FALSE)
    mymont    = H5Fopen(temp.file)
    dummy     = file.remove(temp.file)

  }else if ( file.exists(h5first.gz) ){
    temp.file = file.path(tempdir(),basename(h5first))
    dummy     = gunzip(filename=h5first.gz,destname=temp.file,remove=FALSE)
    mymont    = H5Fopen(temp.file)
    dummy     = file.remove(temp.file)

  }else{
    cat ("Warning: I was looking for the first file to use as template but\n")
    cat ("I have not found it. I will try to use the last one (the one of \n")
    cat ("yearz). This usually happens when not all the analysis (the     \n")
    cat ("first one in particular) are present. If even this one is not   \n")
    cat ("found try to set yearz to the real last H5 file that you have...\n")

    cyear        = sprintf("%4.4i", yeara + floor(ntimes/12) - 1)
    h5first      = paste(inpref,"-Q-",cyear,"-01-00-000000-g01.h5"    ,sep="")
    h5first.bz2  = paste(inpref,"-Q-",cyear,"-01-00-000000-g01.h5.bz2",sep="")
    h5first.gz   = paste(inpref,"-Q-",cyear,"-01-00-000000-g01.h5.gz" ,sep="")

    if ( file.exists(h5first) ){
      mymont    = H5Fopen(h5first)

    }else if ( file.exists(h5first.bz2) ){
      temp.file = file.path(tempdir(),basename(h5first))
      dummy     = bunzip2(filename=h5first.bz2,destname=temp.file,remove=FALSE)
      mymont    = H5Fopen(temp.file)
      dummy     = file.remove(temp.file)

    }else if ( file.exists(h5first.gz) ){
      temp.file = file.path(tempdir(),basename(h5first))
      dummy     = gunzip(filename=h5first.gz,destname=temp.file,remove=FALSE)
      mymont    = H5Fopen(temp.file)
      dummy     = file.remove(temp.file)

    }else{

      cat (" Path: ",dirname (h5first),"\n")
      cat (" File: ",basename(h5first),"\n")
      stop(" File not found...")
    }

  }#end if
  #---------------------------------------------------------------------------------------#


  #---------------------------------------------------------------------------------------#
  #     Start up the list.                                                                #
  #---------------------------------------------------------------------------------------#
  ed      = list()
  #---------------------------------------------------------------------------------------#

  #---------------------------------------------------------------------------------------#
  #     Find all time information.                                                        #
  #---------------------------------------------------------------------------------------#
  runmonths        = montha + sequence(ntimes) - 1
  ed$ntimes        = ntimes
  ed$month         = 1 + (runmonths - 1) %% 12
  ed$year          = yeara - 1 + ceiling(runmonths/12)
  ed$when          = chron(paste(ed$month,1,ed$year,sep="/"))
  ed$tomonth       = chron(ed$when,out.format=c(dates="day-mon-yr",times=NULL))
  ed$toyear        = sort(unique(ed$year))

  #---------------------------------------------------------------------------------------#
  #     Find all the file names.                                                          #
  #---------------------------------------------------------------------------------------#
  cmonth   = sprintf("%2.2i",ed$month)
  cyear    = sprintf("%4.4i",ed$year )
  ed$input = paste(inpref,"-Q-",cyear,"-",cmonth,"-00-000000-g01.h5",sep="")
  #---------------------------------------------------------------------------------------#



  #---------------------------------------------------------------------------------------#
  #    Make a copy of the dimensions to avoid clutter.                                    #
  #---------------------------------------------------------------------------------------#
  ndcycle  = ed$ndcycle
  nzg      = ed$nzg
  nzs      = ed$nzs
  #---------------------------------------------------------------------------------------#


  #---------------------------------------------------------------------------------------#
  # SZPFT -- Size (DBH) and plant functional type (PFT) array.  An extra level is         #
  #          appended to the end, which will hold the sum of all categories.              #
  #                                                                                       #
  #          The initial value depends on the type of variable:                           #
  #          - If absence is equal to zero, then the initial value must be zero. This is  #
  #            normally the case for state variables (e.g. biomass, demographic density,  #
  #            LAI, etc.)                                                                 #
  #          - If absence makes the variable meaningless, then the initial value must     #
  #            be NA.  This is true for plant-derived properties (temperature, mortality, #
  #            gsw, etc.)                                                                 #
  #---------------------------------------------------------------------------------------#
  szpft = list()
  #----- Initial value should be zero. ---------------------------------------------------#
  szpft$agb               = array(data=0 ,dim=c(ntimes,ndbh+1,npft+1))
  szpft$biomass           = array(data=0 ,dim=c(ntimes,ndbh+1,npft+1))
  szpft$lai               = array(data=0 ,dim=c(ntimes,ndbh+1,npft+1))
  szpft$ba                = array(data=0 ,dim=c(ntimes,ndbh+1,npft+1))
  szpft$nplant            = array(data=0 ,dim=c(ntimes,ndbh+1,npft+1))
  szpft$bdead             = array(data=0 ,dim=c(ntimes,ndbh+1,npft+1))
  szpft$balive            = array(data=0 ,dim=c(ntimes,ndbh+1,npft+1))
  szpft$bleaf             = array(data=0 ,dim=c(ntimes,ndbh+1,npft+1))
  szpft$bstem             = array(data=0 ,dim=c(ntimes,ndbh+1,npft+1))
  szpft$broot             = array(data=0 ,dim=c(ntimes,ndbh+1,npft+1))
  szpft$bsapwood          = array(data=0 ,dim=c(ntimes,ndbh+1,npft+1))
  szpft$bseeds            = array(data=0 ,dim=c(ntimes,ndbh+1,npft+1))
  szpft$bstorage          = array(data=0 ,dim=c(ntimes,ndbh+1,npft+1))
  #----- Initial value should be NA. -----------------------------------------------------#
  szpft$gpp               = array(data=NA,dim=c(ntimes,ndbh+1,npft+1))
  szpft$npp               = array(data=NA,dim=c(ntimes,ndbh+1,npft+1))
  szpft$f.bleaf           = array(data=NA,dim=c(ntimes,ndbh+1,npft+1))
  szpft$f.bstem           = array(data=NA,dim=c(ntimes,ndbh+1,npft+1))
  szpft$f.broot           = array(data=NA,dim=c(ntimes,ndbh+1,npft+1))
  szpft$f.bseeds          = array(data=NA,dim=c(ntimes,ndbh+1,npft+1))
  szpft$f.bstorage        = array(data=NA,dim=c(ntimes,ndbh+1,npft+1))
  szpft$acc.growth        = array(data=NA,dim=c(ntimes,ndbh+1,npft+1))
  szpft$mort              = array(data=NA,dim=c(ntimes,ndbh+1,npft+1))
  szpft$dimort            = array(data=NA,dim=c(ntimes,ndbh+1,npft+1))
  szpft$ncbmort           = array(data=NA,dim=c(ntimes,ndbh+1,npft+1))
  #---------------------------------------------------------------------------------------#

  #---------------------------------------------------------------------------------------#
  #  PATCH -- patch level variables, we save as lists because the dimensions vary.    #
  #---------------------------------------------------------------------------------------#
  patch        = list()
  patch$maxh   = list()
  patch$agb    = list()
  patch$bleaf  = list()
  patch$lai    = list()
  patch$gpp    = list()
  patch$nplant = list()
  #---------------------------------------------------------------------------------------#


  dbhds = list()
  dbhds$liana_clss = array(data=0 ,dim=c(ntimes,18,npft+1))
  dbhds$tree_clss  = array(data=0 ,dim=c(ntimes,15,npft+1))

  #----- Copy the polygon-level variable to the main structure. --------------------------#
  ed$szpft  = szpft
  ed$patch  = patch
  ed$dbhds  = dbhds
  # ed$cohort = cohort
  #---------------------------------------------------------------------------------------#


  #---------------------------------------------------------------------------------------#
  return(ed)
  #---------------------------------------------------------------------------------------#

}#end create.monthly
#==========================================================================================#
#==========================================================================================#




#==========================================================================================#
#==========================================================================================#
#     Expand the monthly array so it fits the new times.                                   #
#------------------------------------------------------------------------------------------#
update.monthly <<- function(new.ntimes,old.datum,yeara,inpref){

  #----- Create the new data set. --------------------------------------------------------#
  new.datum = create.monthly(new.ntimes,yeara,inpref)
  #---------------------------------------------------------------------------------------#


  #----- Find out which times to copy. ---------------------------------------------------#
  sel = old.datum$when %in% new.datum$when
  idx = match(old.datum$when[sel],new.datum$when)

  #---------------------------------------------------------------------------------------#
  # SZPFT -- Size (DBH) and plant functional type (PFT) array.  An extra level is         #
  #          appended to the end, which will hold the sum of all categories.              #
  #---------------------------------------------------------------------------------------#
  new.datum$szpft$agb            [idx,,] = old.datum$szpft$agb             [sel,,]
  new.datum$szpft$biomass        [idx,,] = old.datum$szpft$biomass         [sel,,]
  new.datum$szpft$lai            [idx,,] = old.datum$szpft$lai             [sel,,]
  new.datum$szpft$ba             [idx,,] = old.datum$szpft$ba              [sel,,]
  new.datum$szpft$gpp            [idx,,] = old.datum$szpft$gpp             [sel,,]
  new.datum$szpft$npp            [idx,,] = old.datum$szpft$npp             [sel,,]
  new.datum$szpft$nplant         [idx,,] = old.datum$szpft$nplant          [sel,,]
  new.datum$szpft$bdead          [idx,,] = old.datum$szpft$bdead           [sel,,]
  new.datum$szpft$balive         [idx,,] = old.datum$szpft$balive          [sel,,]
  new.datum$szpft$bleaf          [idx,,] = old.datum$szpft$bleaf           [sel,,]
  new.datum$szpft$bstem          [idx,,] = old.datum$szpft$bstem           [sel,,]
  new.datum$szpft$broot          [idx,,] = old.datum$szpft$broot           [sel,,]
  new.datum$szpft$bsapwood       [idx,,] = old.datum$szpft$bsapwood        [sel,,]
  new.datum$szpft$bstorage       [idx,,] = old.datum$szpft$bstorage        [sel,,]
  new.datum$szpft$f.bstorage     [idx,,] = old.datum$szpft$f.bstorage      [sel,,]
  new.datum$szpft$f.bleaf        [idx,,] = old.datum$szpft$f.bleaf         [sel,,]
  new.datum$szpft$f.bstem        [idx,,] = old.datum$szpft$f.bstem         [sel,,]
  new.datum$szpft$f.broot        [idx,,] = old.datum$szpft$f.broot         [sel,,]
  new.datum$szpft$f.bseeds       [idx,,] = old.datum$szpft$f.bseeds        [sel,,]
  new.datum$szpft$acc.growth     [idx,,] = old.datum$szpft$acc.growth      [sel,,]
  new.datum$szpft$mort           [idx,,] = old.datum$szpft$mort            [sel,,]
  new.datum$szpft$dimort         [idx,,] = old.datum$szpft$dimort          [sel,,]
  new.datum$szpft$ncbmort        [idx,,] = old.datum$szpft$ncbmort         [sel,,]
  #---------------------------------------------------------------------------------------#

  new.datum$dbhds$liana_clss     [idx,,] = old.datum$dbhds$liana_clss      [sel,,]
  new.datum$dbhds$tree_clss      [idx,,] = old.datum$dbhds$tree_clss       [sel,,]

  #---------------------------------------------------------------------------------------#
  #  PATCH -- patch level variables, we save as lists because the dimensions vary.    #
  #---------------------------------------------------------------------------------------#
  new.datum$patch$ipa           = old.datum$patch$ipa
  new.datum$patch$age           = old.datum$patch$age
  new.datum$patch$area          = old.datum$patch$area
  new.datum$patch$nep           = old.datum$patch$nep
  new.datum$patch$lai           = old.datum$patch$lai
  new.datum$patch$agb           = old.datum$patch$agb
  new.datum$patch$ba            = old.datum$patch$ba
  new.datum$patch$gpp           = old.datum$patch$gpp
  new.datum$patch$npp           = old.datum$patch$npp
  new.datum$patch$maxh          = old.datum$patch$maxh
  new.datum$patch$agb           = old.datum$patch$agb
  new.datum$patch$bleaf         = old.datum$patch$bleaf
  new.datum$patch$lai           = old.datum$patch$lai
  new.datum$patch$gpp           = old.datum$patch$gpp
  new.datum$patch$nplant        = old.datum$patch$nplant
  #---------------------------------------------------------------------------------------#

  #---------------------------------------------------------------------------------------#
  #  PATCH -- patch level variables, we save as lists because the dimensions vary.        #
  #---------------------------------------------------------------------------------------#
  patch        = list()
  patch$maxh   = list()
  patch$agb    = list()
  patch$bleaf  = list()
  patch$lai    = list()
  patch$gpp    = list()
  patch$nplant = list()
  #---------------------------------------------------------------------------------------#

  #---------------------------------------------------------------------------------------#
  #     Send the data back.                                                               #
  #---------------------------------------------------------------------------------------#
  return(new.datum)
  #---------------------------------------------------------------------------------------#
}#end update
#==========================================================================================#
#==========================================================================================#
