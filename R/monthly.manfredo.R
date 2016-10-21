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
    mymont    = H5Fopen(h5first)
    dummy     = file.remove(temp.file)

  }else if ( file.exists(h5first.gz) ){
    temp.file = file.path(tempdir(),basename(h5first))
    dummy     = gunzip(filename=h5first.gz,destname=temp.file,remove=FALSE)
    mymont    = H5Fopen(h5first)
    dummy     = file.remove(temp.file)

  }else{
    cat (" Path: ",dirname (h5first),"\n")
    cat (" File: ",basename(h5first),"\n")
    stop(" File not found...")

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
  ed$month         = 1 + (runmonths-1) %% 12
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
  # emean -- variables that we can either compare directly with observations, or are      #
  #          or that may be used to draw time series.   They don't need to be really      #
  #          monthly means, but you should put only the variables that make sense to be   #
  #          plotted in simple time series (with no PFT or DBH information).              #
  #---------------------------------------------------------------------------------------#
  emean = list()
  emean$gpp                     = rep(NA,times=ntimes)
  emean$nep                     = rep(NA,times=ntimes)
  emean$evap                    = rep(NA,times=ntimes)
  emean$npp                     = rep(NA,times=ntimes)
  emean$nplant                  = rep(NA,times=ntimes)
  emean$agb                     = rep(NA,times=ntimes)
  emean$biomass                 = rep(NA,times=ntimes)
  emean$lai                     = rep(NA,times=ntimes)
  emean$area                    = rep(NA,times=ntimes)
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
  #----- Initial value should be NA. -----------------------------------------------------#
  szpft$gpp               = array(data=NA,dim=c(ntimes,ndbh+1,npft+1))
  szpft$npp               = array(data=NA,dim=c(ntimes,ndbh+1,npft+1))
  szpft$f.bleaf           = array(data=NA,dim=c(ntimes,ndbh+1,npft+1))
  szpft$f.bstem           = array(data=NA,dim=c(ntimes,ndbh+1,npft+1))
  szpft$f.broot           = array(data=NA,dim=c(ntimes,ndbh+1,npft+1))
  #---------------------------------------------------------------------------------------#

  #---------------------------------------------------------------------------------------#
  #  PATCH -- patch level variables, we save as lists because the dimensions vary.    #
  #---------------------------------------------------------------------------------------#
  patch               = list()
  patch$maxh          = list()
  #---------------------------------------------------------------------------------------#




  #----- Cohort level, we save as lists because the dimensions vary. ---------------------#
  cohort                = list()
  cohort$ipa            = list()
  cohort$ico            = list()
  cohort$area           = list()
  cohort$dbh            = list()
  cohort$age            = list()
  cohort$pft            = list()
  cohort$nplant         = list()
  cohort$height         = list()
  cohort$ba             = list()
  cohort$agb            = list()
  cohort$lai            = list()
  cohort$gpp            = list()
  cohort$npp            = list()
  cohort$balive         = list()
  cohort$bdead          = list()
  cohort$bleaf          = list()
  cohort$bstem          = list()
  cohort$broot          = list()
  cohort$bsapwood       = list()
  cohort$f.bleaf        = list()
  cohort$f.bstem        = list()
  cohort$f.broot        = list()
  #---------------------------------------------------------------------------------------#





  #----- Copy the polygon-level variable to the main structure. --------------------------#
  ed$szpft  = szpft
  ed$patch  = patch
  ed$cohort = cohort
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
  #---------------------------------------------------------------------------------------#
  # emean -- variables that we can either compare directly with observations, or are      #
  #          or that may be used to draw time series.   They don't need to be really      #
  #          monthly means, but you should put only the variables that make sense to be   #
  #          plotted in simple time series (with no PFT or DBH information).              #
  #---------------------------------------------------------------------------------------#
  new.datum$emean$gpp               [idx ] = old.datum$emean$gpp                 [sel ]
  new.datum$emean$npp               [idx ] = old.datum$emean$npp                 [sel ]
  new.datum$emean$nep               [idx ] = old.datum$emean$nep                 [sel ]
  new.datum$emean$evap              [idx ] = old.datum$emean$evap                [sel ]
  new.datum$emean$agb               [idx ] = old.datum$emean$agb                 [sel ]
  new.datum$emean$biomass           [idx ] = old.datum$emean$biomass             [sel ]
  new.datum$emean$nplant            [idx ] = old.datum$emean$nplant              [sel ]
  new.datum$emean$lai               [idx ] = old.datum$emean$lai                 [sel ]
  new.datum$emean$area              [idx ] = old.datum$emean$area                [sel ]
  #---------------------------------------------------------------------------------------#



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
  new.datum$szpft$f.bleaf        [idx,,] = old.datum$szpft$f.bleaf         [sel,,]
  new.datum$szpft$f.bstem        [idx,,] = old.datum$szpft$f.bstem         [sel,,]
  new.datum$szpft$f.broot        [idx,,] = old.datum$szpft$f.broot         [sel,,]
  #---------------------------------------------------------------------------------------#



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
  new.datum$patch$maxh           = old.datum$patch$maxh
  #---------------------------------------------------------------------------------------#



  #----- Cohort level, we save as lists because the dimensions vary. ---------------------#
  new.datum$cohort$ipa              = old.datum$cohort$ipa
  new.datum$cohort$ico              = old.datum$cohort$ico
  new.datum$cohort$area             = old.datum$cohort$area
  new.datum$cohort$dbh              = old.datum$cohort$dbh
  new.datum$cohort$age              = old.datum$cohort$age
  new.datum$cohort$pft              = old.datum$cohort$pft
  new.datum$cohort$nplant           = old.datum$cohort$nplant
  new.datum$cohort$height           = old.datum$cohort$height
  new.datum$cohort$ba               = old.datum$cohort$ba
  new.datum$cohort$agb              = old.datum$cohort$agb
  new.datum$cohort$lai              = old.datum$cohort$lai
  new.datum$cohort$gpp              = old.datum$cohort$gpp
  new.datum$cohort$npp              = old.datum$cohort$npp
  new.datum$cohort$balive           = old.datum$cohort$balive
  new.datum$cohort$bdead            = old.datum$cohort$bdead
  new.datum$cohort$bleaf            = old.datum$cohort$bleaf
  new.datum$cohort$bstem            = old.datum$cohort$bstem
  new.datum$cohort$broot            = old.datum$cohort$broot
  new.datum$cohort$bsapwood         = old.datum$cohort$bsapwood
  new.datum$cohort$f.bleaf          = old.datum$cohort$f.bleaf
  new.datum$cohort$f.bstem          = old.datum$cohort$f.bstem
  new.datum$cohort$f.broot          = old.datum$cohort$f.broot
  #---------------------------------------------------------------------------------------#

  #---------------------------------------------------------------------------------------#
  #  PATCH -- patch level variables, we save as lists because the dimensions vary.    #
  #---------------------------------------------------------------------------------------#
  patch               = list()
  patch$maxh          = list()
  #---------------------------------------------------------------------------------------#


  #----- Cohort level, we save as lists because the dimensions vary. ---------------------#
  cohort                = list()
  cohort$ipa            = list()
  cohort$ico            = list()
  cohort$area           = list()
  cohort$dbh            = list()
  cohort$age            = list()
  cohort$pft            = list()
  cohort$nplant         = list()
  cohort$height         = list()
  cohort$ba             = list()
  cohort$agb            = list()
  cohort$lai            = list()
  cohort$gpp            = list()
  cohort$npp            = list()
  cohort$balive         = list()
  cohort$bdead          = list()
  cohort$bleaf          = list()
  cohort$bstem          = list()
  cohort$broot          = list()
  cohort$bsapwood       = list()
  cohort$f.bleaf        = list()
  cohort$f.bstem        = list()
  cohort$f.broot        = list()
  #---------------------------------------------------------------------------------------#


  #---------------------------------------------------------------------------------------#
  #     Send the data back.                                                               #
  #---------------------------------------------------------------------------------------#
  return(new.datum)
  #---------------------------------------------------------------------------------------#
}#end update
#==========================================================================================#
#==========================================================================================#
