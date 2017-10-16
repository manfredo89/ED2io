#------------------------------------------------------------------------------------------#
#     This script reads in the version 2.0 data from Natalia Coupe and creates the         #
# meteorological drivers.                                                                  #
#------------------------------------------------------------------------------------------#
rm(list=ls())
graphics.off()
#------------------------------------------------------------------------------------------#



#----- Default settings. ------------------------------------------------------------------#
here    = getwd()                  # Current directory.
outpath = "/Users/manfredo/Desktop/r_minimal/ED2io/R/INPUT/gen_init+bnd_cond/met_driver/output"
#------------------------------------------------------------------------------------------#



#------------------------------------------------------------------------------------------#
#    List of places, with the name, first and last full years, and the output variable .   #
#------------------------------------------------------------------------------------------#
place       = list()
place[[ 1]] = list( name     = "santarem_km83"
                    , longname = "Santarem - Km 83"
                    , lon      = -54.971
                    , lat      =  -3.018
                    , h5pref   = "Santarem_Km83"
                    , height   = 64
                    , dtdat    = 3600.
                    , fill.in  = FALSE
)#end if
place[[ 2]] = list( name     = "bci"
                    , longname = "Barro Colorado Island"
                    , lon      = -79.828
                    , lat      =  9.152
                    , h5pref   = "bci"
                    , height   = 64
                    , dtdat    = 3600.
                    , fill.in  = TRUE
)#end if
#------------------------------------------------------------------------------------------#

#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#     No need to change anything beyond this point unless you are developing the code.     #
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#



#----- Find the number of places to make. -------------------------------------------------#
nplaces      = length(place)
#------------------------------------------------------------------------------------------#



#----- Avoid unecessary and extremely annoying beeps. -------------------------------------#
options(locatorBell=FALSE)
#------------------------------------------------------------------------------------------#



#----- Load packages. ---------------------------------------------------------------------#
isok = require(chron)
isok = require(rhdf5)
#------------------------------------------------------------------------------------------#



#----- Load useful functions. -------------------------------------------------------------#
source("/Users/manfredo/Desktop/r_minimal/ED2io/R/INPUT/gen_init+bnd_cond/met_driver/timeutils.r")
source("/Users/manfredo/Desktop/r_minimal/ED2io/R/INPUT/gen_init+bnd_cond/met_driver/radutils.r")
#------------------------------------------------------------------------------------------#


#----- Check that output path exists. -----------------------------------------------------#
if (! file.exists(outpath)) dir.create(outpath)
#------------------------------------------------------------------------------------------#


for (p in sequence(nplaces)){
  #----- Copy structure to local variables. ----------------------------------------------#
  info       = place[[p]]
  file.in    = file.path(here,paste("filled_",info$name,".txt",sep=""))
  #---------------------------------------------------------------------------------------#
  
  
  #---------------------------------------------------------------------------------------#
  #     Read the data.                                                                    #
  #---------------------------------------------------------------------------------------#
  datum = read.table(file=file.in,sep=",",na=NA,header=TRUE,skip=47)
  cat(" + Processing data from ",info$longname,"...","\n",sep="")
  #---------------------------------------------------------------------------------------#
  #=======================================================================================#
  #=======================================================================================#
  
  
  #---------------------------------------------------------------------------------------#
  #     Find the dates.                                                                   #
  #---------------------------------------------------------------------------------------#
  datum$when    = chron( paste(datum$month,datum$day,datum$year,sep="/")
                         , paste(datum$hour ,datum$min,datum$sec ,sep=":") )
  datum$today   = chron( paste(datum$month,datum$day,datum$year,sep="/") )
  datum$tomonth = chron( paste(datum$month,        1,datum$year,sep="/") )
  #---------------------------------------------------------------------------------------#
  
  
  
  
  #---------------------------------------------------------------------------------------#
  #     Find the zenith angle, to split radiation into components.                        #
  #---------------------------------------------------------------------------------------#
  zen           = ed.zen(lon=info$lon,lat=info$lat,when=datum$when,ed21=TRUE
                         ,zeronight=FALSE,meanval=TRUE,imetavg=1,nmean=60     )
  datum$cosz    = zen$cosz
  #---------------------------------------------------------------------------------------#
  
  
  
  
  #---------------------------------------------------------------------------------------#
  #     Find the zenith angle, to split radiation into components.                        #
  #---------------------------------------------------------------------------------------#
  zen           = ed.zen(lon=info$lon,lat=info$lat,when=datum$when,ed21=TRUE
                         ,zeronight=FALSE,meanval=TRUE,imetavg=1,nmean=15     )
  datum$cosz    = zen$cosz
  #---------------------------------------------------------------------------------------#
  
  
  
  
  #---------------------------------------------------------------------------------------#
  #     Split the incoming radiation into components, or estimate NIR from PAR if PAR     #
  # measurements are available.                                                           #
  #---------------------------------------------------------------------------------------#
  prss = mean(datum$atm.prss)
  rad  = rshort.bdown(rad.in=datum$rshort,atm.prss=datum$atm.prss,cosz=datum$cosz)
  datum$par.beam = rad$par.beam
  datum$par.diff = rad$par.diff
  datum$nir.beam = rad$nir.beam
  datum$nir.diff = rad$nir.diff
  #---------------------------------------------------------------------------------------#
  
  
  
  
  #---------------------------------------------------------------------------------------#
  #      Decompose wind.                                                                  #
  #---------------------------------------------------------------------------------------#
  trigo          = (270. - datum$atm.vdir) * pio180
  datum$atm.uspd = datum$atm.vels * cos(trigo)
  datum$atm.vspd = datum$atm.vels * sin(trigo)
  #---------------------------------------------------------------------------------------#
  
  
  
  
  
  
  #=======================================================================================#
  #=======================================================================================#
  #=======================================================================================#
  #=======================================================================================#
  #=======================================================================================#
  #=======================================================================================#
  #=======================================================================================#
  #=======================================================================================#
  #     Make ED output.                                                                   #
  #---------------------------------------------------------------------------------------#
  cat(" + Making ED-friendly output files... \n")
  
  #----- Make sure that the output directory exists, and if not, create it. --------------#
  siteroot = file.path(outpath,info$h5pref)
  if (! file.exists(siteroot)) dir.create(siteroot)
  #---------------------------------------------------------------------------------------#
  
  #----- List all possible unique month/year combinations. -------------------------------#
  unique.tomonth   = unique(datum$tomonth)
  n.unique.tomonth = length(unique.tomonth)
  #---------------------------------------------------------------------------------------#
  
  for (um in sequence(n.unique.tomonth)){
    monyear.now = unique.tomonth[um]
    
    #----- Get current month and year, 3-letter month, and number of days in the month. -#
    month.now   = nummonths(monyear.now)
    year.now    = numyears (monyear.now)
    daymax.now  = daymax   (month=month.now,year=year.now)
    month.label = toupper(month.abb[month.now])
    year.label  = sprintf("%4.4i",year.now)
    #------------------------------------------------------------------------------------#
    
    
    #----- Print banner to entertain the user. ------------------------------------------#
    cat("   - Checking data from ",month.name[month.now]," ",year.now,"...","\n",sep="")
    #------------------------------------------------------------------------------------#
    
    
    #----- Find the indices of data that belong to this month and year. -----------------#
    sel    = datum$month == month.now & datum$year == year.now
    #------------------------------------------------------------------------------------#
    
    
    #----- Check that all data are there. -----------------------------------------------#
    nsel      = sum(sel)
    nexpected = daymax.now * day.sec / info$dtdat
    #------------------------------------------------------------------------------------#
    
    # Manfredo added some corrections and filling for long wave radiation
    # https://earthscience.stackexchange.com/questions/2360/how-do-i-convert-specific-humidity-to-relative-humidity
    
    if(info$fill.in){
      
      source("/Users/manfredo/Desktop/r_minimal/ED2io/R/rconstants.r")
      source("/Users/manfredo/Desktop/r_minimal/ED2io/R/INPUT/gen_init+bnd_cond/met_driver/marthews.rlong.r")
      source("/Users/manfredo/Desktop/r_minimal/ED2io/R/INPUT/gen_init+bnd_cond/met_driver/thermlib.r")
      cat ("WARNING: I am converting relative humidity to specific humidity!","\n")
      datum$atm.shv = (100 * datum$atm.rhv * exp(17.67*(datum$atm.tmp - t00) / (datum$atm.tmp - 29.65))) / (0.263 * datum$atm.prss)
      datum$atm.shv2 = ep / ( datum$atm.prss * eslif(datum$atm.tmp) / datum$atm.rhv + (1. - ep))
      
      
      if(all(is.na(datum$rlong.in))){
        
        cat ("WARNING: I did not find the long wave radiation!!", " \n\n")
        cat ("Attempting to fill the data with CDO algorithm.","\n")
        cat ("Reference: Marthews et al. (2012)","\n")
        
        datum$atm.pvap = datum$atm.rhv * eslif(datum$atm.tmp)
        datum = rlong.in.mmi.predict(datum=datum)
      }
    }
    
    
    #----- If the data are complete, make the output file. ------------------------------#
    if (nsel == nexpected){
      cat("     * Data series is complete, making the arrays...","\n")
      #---------------------------------------------------------------------------------#
      #      Create the matrices that will have the data.  These will use ED/NCEP name  #
      # convention, and will have a fake 2x2 matrix with the same data because the      #
      # tower may be needed in a site that is nearby but not with the same longitude    #
      # and latitude.                                                                   #
      #---------------------------------------------------------------------------------#
      # dlwrf = aperm(a=array(datum$rlong.in[sel],dim=c(nsel,2,2)),perm=c(2,3,1))
      # nbdsf = aperm(a=array(datum$nir.beam[sel],dim=c(nsel,2,2)),perm=c(2,3,1))
      # nddsf = aperm(a=array(datum$nir.diff[sel],dim=c(nsel,2,2)),perm=c(2,3,1))
      # prate = aperm(a=array(datum$rain    [sel],dim=c(nsel,2,2)),perm=c(2,3,1))
      # pres  = aperm(a=array(datum$atm.prss[sel],dim=c(nsel,2,2)),perm=c(2,3,1))
      # sh    = aperm(a=array(datum$atm.shv [sel],dim=c(nsel,2,2)),perm=c(2,3,1))
      # tmp   = aperm(a=array(datum$atm.tmp [sel],dim=c(nsel,2,2)),perm=c(2,3,1))
      # ugrd  = aperm(a=array(datum$atm.uspd[sel],dim=c(nsel,2,2)),perm=c(2,3,1))
      # vbdsf = aperm(a=array(datum$par.beam[sel],dim=c(nsel,2,2)),perm=c(2,3,1))
      # vddsf = aperm(a=array(datum$par.diff[sel],dim=c(nsel,2,2)),perm=c(2,3,1))
      # vgrd  = aperm(a=array(datum$atm.vspd[sel],dim=c(nsel,2,2)),perm=c(2,3,1))
      dlwrf = array(datum$rlong.in[sel],dim=c(nsel,2,2))
      nbdsf = array(datum$nir.beam[sel],dim=c(nsel,2,2))
      nddsf = array(datum$nir.diff[sel],dim=c(nsel,2,2))
      prate = array(datum$rain    [sel],dim=c(nsel,2,2))
      pres  = array(datum$atm.prss[sel],dim=c(nsel,2,2))
      sh    = array(datum$atm.shv [sel],dim=c(nsel,2,2))
      tmp   = array(datum$atm.tmp [sel],dim=c(nsel,2,2))
      ugrd  = array(datum$atm.uspd[sel],dim=c(nsel,2,2))
      vbdsf = array(datum$par.beam[sel],dim=c(nsel,2,2))
      vddsf = array(datum$par.diff[sel],dim=c(nsel,2,2))
      vgrd  = array(datum$atm.vspd[sel],dim=c(nsel,2,2))
      #---------------------------------------------------------------------------------#
      
      #---------------------------------------------------------------------------------#
      #     Make sure that all data are there.  If not, crash!                          #
      #     Manfredo: this was nwver crashing cause dlwrf was of type character         #
      #---------------------------------------------------------------------------------#
      
      if (any(is.na(dlwrf))) stop("dlwrf has missing values!")
      if (any(is.na(nbdsf))) stop("nbdsf has missing values!")
      if (any(is.na(nddsf))) stop("nddsf has missing values!")
      if (any(is.na(prate))) stop("prate has missing values!")
      if (any(is.na(pres ))) stop("pres  has missing values!")
      if (any(is.na(sh   ))) stop("sh    has missing values!")
      if (any(is.na(tmp  ))) stop("tmp   has missing values!")
      if (any(is.na(ugrd ))) stop("ugrd  has missing values!")
      if (any(is.na(vbdsf))) stop("vbdsf has missing values!")
      if (any(is.na(vddsf))) stop("vddsf has missing values!")
      if (any(is.na(vgrd ))) stop("vgrd  has missing values!")
      #---------------------------------------------------------------------------------#
      
      
      #---------------------------------------------------------------------------------#
      #     Make the dataset.                                                           #
      #---------------------------------------------------------------------------------#
      thisfile = file.path(siteroot
                           ,paste(info$h5pref,"_",year.label,month.label,".h5",sep=""))
      cat("     * Saving data to ",basename(thisfile),"...","\n",sep="")
      
      h5save(file=thisfile,dlwrf,nbdsf,nddsf,prate,pres,sh,tmp,ugrd,vbdsf,vddsf,vgrd)
      H5close()
      #---------------------------------------------------------------------------------#
    }else{
      stop (" ---> Could not find all the data for this month!")
    }#end if
  }#end for
  #---------------------------------------------------------------------------------------#
  
  
  #---------------------------------------------------------------------------------------#
  #     Make the header for this experiment.                                              #
  #---------------------------------------------------------------------------------------#
  header  = file.path(siteroot,"ED_MET_DRIVER_HEADER")
  info    = c("# See README at the bottom of this file."
              ,"1"
              ,paste(siteroot,"/",info$h5pref,"_",sep="")
              ,paste(2,2,1.0,1.0,info$lon,info$lat)
              ,"12"
              ,paste("'hgt' 'dlwrf' 'nbdsf' 'nddsf' 'prate' 'pres' 'sh' 'tmp' 'ugrd' 'vbdsf' 'vddsf' 'vgrd'",sep="")
              ,paste(c(info$height,rep(info$dtdat,times=11)),collapse=" ")
              ,paste(" 4 1 1 1 0 1 1 1 1 1 1 1",sep="")
              ,paste(" ")
              ,paste("!===========================================================!")
              ,paste("! README                                                    !")
              ,paste("!===========================================================!")
              ,paste("!     The header of the meteorological driver must contain  !")
              ,paste("! the following lines:                                      !")
              ,paste("!                                                           !")
              ,paste("! Line  1 : Banner, it will not be read;                    !")
              ,paste("! Line  2 : Number of file formats, hereafter N;            !")
              ,paste("! Lines 3+: For each of the N formats, add the following    !")
              ,paste("!           lines, going through a-f for the first format,  !")
              ,paste("!           then through a-f for the second format and so   !")
              ,paste("!            on:                                            !")
              ,paste("!    a. Prefixes of the file format;                        !")
              ,paste("!    b. nlon, nlat, deltalon, deltalat, lon0, lat0.  If     !")
              ,paste("!       lon and lat are also variables, only nlon and nlat  !")
              ,paste("!       will be used;                                       !")
              ,paste("!    c. Number of variables contained in this format;       !")
              ,paste("!    d. List of variables for each format (see Table 1);    !")
              ,paste("!    e. Frequency at which vares are updated, or the        !")
              ,paste("!       constant value if the variable type is 4;           !")
              ,paste("!    f. Variable type (see Table 2);                        !")
              ,paste("!                                                           !")
              ,paste("!===========================================================!")
              ,paste("! Table 1. Variable names recognized by ED.                 !")
              ,paste("!===========================================================!")
              ,paste("! -> lon    -  Longitude                        [    deg]   !")
              ,paste("! -> lat    -  Latitude                         [    deg]   !")
              ,paste("! -> hgt    -  Reference height                 [  m AGL]   !")
              ,paste("! -> tmp    -  Air temperature                  [      K]   !")
              ,paste("! -> pres   -  Pressure                         [     Pa]   !")
              ,paste("! -> sh     -  Specific humidity                [  kg/kg]   !")
              ,paste("! -> ugrd   -  Zonal wind                       [    m/s]   !")
              ,paste("! -> vgrd   -  Zonal wind                       [    m/s]   !")
              ,paste("! -> prate  -  Precipitation rate               [kg/m2/s]   !")
              ,paste("! -> dlwrf  -  Downward long wave radiation     [   W/m2]   !")
              ,paste("! -> nbdsf  -  Near-IR beam radiation           [   W/m2]   !")
              ,paste("! -> nddsf  -  Near-IR diffuse radiation        [   W/m2]   !")
              ,paste("! -> vbdsf  -  Visible beam radiation           [   W/m2]   !")
              ,paste("! -> vddsf  -  Visible beam radiation           [   W/m2]   !")
              ,paste("!===========================================================!")
              ,paste("!                                                           !")
              ,paste("!===========================================================!")
              ,paste("! Table 2. Variable types recognized by ED.                 !")
              ,paste("!===========================================================!")
              ,paste("!                                                           !")
              ,paste("! 0. Read gridded data - no time interpolation;             !")
              ,paste("! 1. Read gridded data - with time interpolatation;         !")
              ,paste("! 2. Read gridded data that is constant in time.            !")
              ,paste("!    If any of this is lon or lat, then deltalon, deltalat  !")
              ,paste("!    lon0, and lat0 will be ignored;                        !")
              ,paste("! 3. Read one value representing the whole grid, no time    !")
              ,paste("!   interpolation;                                          !")
              ,paste("! 4. Specify a constant for all polygons, constant in time. !")
              ,paste("!    In this case, give the constant value at line 'e'      !")
              ,paste("!    instead of the frequency.                              !")
              ,paste("!===========================================================!")
  )#end c
  #---------------------------------------------------------------------------------------#
  
  
  
  #---------------------------------------------------------------------------------------#
  #     Write the header.                                                                 #
  #---------------------------------------------------------------------------------------#
  write (x=info,file=header,ncolumns=1,append=FALSE,sep=" ")
  #---------------------------------------------------------------------------------------#
  #=======================================================================================#
  #=======================================================================================#
  
  rm(datum  )
}#end for
#------------------------------------------------------------------------------------------#
