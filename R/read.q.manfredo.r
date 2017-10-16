#==========================================================================================#
#==========================================================================================#
#     This function reads the ED2 monthly mean files that contain mean diurnal cycle.      #
#   Inputs:                                                                                #
#   - datum   -- The monthly structure that will contain the data.  It must be initialised #
#                by create.monthly, otherwise it won't work.                               #
#   - ntimes  -- Total number of times (including previously loaded times).                #
#   - tresume -- The first time to read (in case data have been partially loaded.          #
#                                                                                          #
#                                                                                          #
#                                                                                          #
#  NOTES: aroudn line 164 there's the variable baliveconow which is currently unused.      #
#  It should probably be kept trace of, especially when calculating the biomass fractions  #
#                                                                                          #
#                                                                                          #
#------------------------------------------------------------------------------------------#
read.q.files <<- function(datum,ntimes,tresume=1,sasmonth=5){

  #---------------------------------------------------------------------------------------#
  #     Copy the variables to scratch lists, we will copy them back once we are done.     #
  #---------------------------------------------------------------------------------------#
  emean  = datum$emean
  szpft  = datum$szpft
  cohort = datum$cohort
  patch  = datum$patch
  #---------------------------------------------------------------------------------------#


  #---------------------------------------------------------------------------------------#
  #     Loop over all times that haven't been read yet.                                   #
  #---------------------------------------------------------------------------------------#
  for (m in tresume:ntimes){

    #----- Print a banner to entertain the bored user staring at the screen. ------------#
    if (m == tresume | datum$month[m] == 1){
      cat("    - Reading data from year ",datum$year[m],"...","\n")
    }#end if
    #------------------------------------------------------------------------------------#


    #------------------------------------------------------------------------------------#
    #     Number of days in a month.                                                     #
    #------------------------------------------------------------------------------------#
    mondays   = daymax(datum$when[m])
    thismonth = datum$month[m]
    lastmonth = 1 + (thismonth - 2) %% 12
    thisyear  = datum$year [m]
    #------------------------------------------------------------------------------------#


    #----- Read data and close connection immediately after. ----------------------------#
    h5file       = datum$input[m]
    h5file.bz2   = paste(datum$input[m],"bz2",sep=".")
    h5file.gz    = paste(datum$input[m],"gz" ,sep=".")
    if (file.exists(h5file)){
      cat(h5file,"\n")
      mymont    = H5Fopen(h5file)

    }else if(file.exists(h5file.bz2)){
      temp.file = file.path(tempdir(),basename(h5file))
      dummy     = bunzip2(filename=h5file.bz2,destname=temp.file,remove=FALSE)
      mymont    = H5Fopen(temp.file)
      dummy     = file.remove(temp.file)

    }else if(file.exists(h5file.gz)){
      temp.file = file.path(tempdir(),basename(h5file))
      dummy     = gunzip(filename=h5file.gz,destname=temp.file,remove=FALSE)
      mymont    = H5Fopen(temp.file)
      dummy     = file.remove(temp.file)
    }else{
      cat (" - File      : ",h5file    ,"\n")
      cat (" - File (bz2): ",h5file.bz2,"\n")
      stop(" Neither the expanded nor the compressed files were found!")

    }#end if
    #------------------------------------------------------------------------------------#

    #----- Load the simple variables_ ---------------------------------------------------#
    emean$gpp             [m] =   mymont$MMEAN_GPP_PY
    emean$npp             [m] =   mymont$MMEAN_NPP_PY
    emean$nep             [m] =   mymont$MMEAN_NEP_PY
    emean$evap            [m] = ( mymont$MMEAN_VAPOR_GC_PY
                                  + mymont$MMEAN_VAPOR_LC_PY
                                  + mymont$MMEAN_VAPOR_WC_PY ) * day.sec


    #---- Read in the site-level area. --------------------------------------------------#
    areasi     = mymont$AREA_SI
    npatches   = mymont$SIPA_N
    #------------------------------------------------------------------------------------#


    #----- Read a few patch-level variables. --------------------------------------------#
    areapa      = mymont$AREA * rep(areasi,times=npatches)
    areapa      = areapa / sum(areapa)
    ipa         = sequence(mymont$NPATCHES_GLOBAL)
    agepa       = mymont$AGE
    #------------------------------------------------------------------------------------#

    #------------------------------------------------------------------------------------#
    #     Get the total number of cohorts.                                               #
    #------------------------------------------------------------------------------------#
    ncohorts    = mymont$PACO_N
    #ipaconow is the index of the patch to which the cohort belongs
    ipaconow    = rep(sequence(mymont$NPATCHES_GLOBAL),times=mymont$PACO_N)
    #icoconow is the index of the cohort inside the patch (to what patch belong the nth cohort)
    icoconow    = unlist(sapply(X = ncohorts, FUN = sequence))
    #idx is the index of the patch (i don't understand the operation though)
    idx         = match(unique(ipaconow),sequence(mymont$NPATCHES_GLOBAL))
    #------------------------------------------------------------------------------------#

    #----- Used pft and derived properties ----------------------------------------#
    pftconow = mymont$PFT
    pftuse   = sort(unique(pftconow))
    npftuse  = length(pftuse)
    #------------------------------------------------------------------------------#



    #------------------------------------------------------------------------------------#
    #      Build the cohort-level lists if this is the right month.                      #
    #------------------------------------------------------------------------------------#
    plab = paste( "y",sprintf("%4.4i",thisyear )
                  , "m",sprintf("%2.2i",thismonth),sep="")
    patch$maxh         [[plab]] = array(data=0. ,dim=c(mymont$NPATCHES_GLOBAL, npftuse))

    if (any(ncohorts >0)){
      #----- Find some auxiliary patch-level properties. -------------------------------#
      #sum the lai value of cohorts for each patch
      lai.pa         = tapply(X = mymont$MMEAN_LAI_CO, INDEX = ipaconow, FUN = sum)

      #---------------------------------------------------------------------------------#
      #     Load some cohort-level structures that we will use multiple times.          #
      #---------------------------------------------------------------------------------#
      dbhconow          = mymont$DBH
      dbhconow.lastmon  = mymont$DBH * exp(-pmax(0,mymont$DLNDBH_DT/12))
      nplantconow       = mymont$NPLANT
      heightconow       = mymont$HITE
      baconow           = mymont$BA_CO
      agbconow          = mymont$AGB_CO
      laiconow          = mymont$MMEAN_LAI_CO
      gppconow          = mymont$MMEAN_GPP_CO
      nppconow          = mymont$MMEAN_NPP_CO

      #----- Find biomass of some tissues. ------------------------------------------#
      bdeadconow        = mymont$BDEAD
      bleafconow        = mymont$MMEAN_BLEAF_CO
      bsapwoodconow     = mymont$BSAPWOODA+mymont$BSAPWOODB
      if (all(mymont$MMEAN_BROOT_CO == 0)){
        bfrootconow    = ( dbh2bl(dbh=dbhconow.lastmon,ipft=pftconow)
                           * pft$qroot[pftconow] )
      }else{
        bfrootconow    = mymont$MMEAN_BROOT_CO
      }#end if
      bcrootconow       = mymont$BSAPWOODB + (1. - pft$agf.bs[pftconow]) * bdeadconow
      bstemconow        = mymont$BSAPWOODA +       pft$agf.bs[pftconow]  * bdeadconow
      brootconow        = bfrootconow + bcrootconow
      baliveconow       = bleafconow  + bfrootconow + bsapwoodconow
      #------------------------------------------------------------------------------#

      #----- Find the variables that must be rendered extensive. -----------------------#
      maxh.pa   = (data.frame(tapply(X= heightconow, INDEX = list(ipaconow, pftconow), FUN = max)))[sequence(npftuse)]
      #---------------------------------------------------------------------------------#

      #---------------------------------------------------------------------------------#
      #     Copy the data back to the patch.                                            #
      #---------------------------------------------------------------------------------#
      patch$maxh      [[plab]] = maxh.pa
      #---------------------------------------------------------------------------------#
    }#end if
    #------------------------------------------------------------------------------------#


    #------------------------------------------------------------------------------------#
    #     Read the cohort-level variables.  Because empty patchs do exist (deserts),     #
    # we must check whether there is any cohort to be read.  If not, assign NA to        #
    # all variables.                                                                     #
    #------------------------------------------------------------------------------------#
    one.cohort = sum(ncohorts) == 1
    one.patch  = sum(npatches) == 1

    if (one.cohort || one.patch){

      cat (" ##########################################################################
             ################                                          ################
             ################     WARNING: ONLY ONE COHORT FOUND       ################
             ################     CHECK READ.Q.MANFREDO FILE           ################
             ################                                          ################
             ##########################################################################
           ")
    }

    if (any (ncohorts > 0)){

      areaconow  = rep(areapa,times=ncohorts)

      #----- Define the DBH classes. ---------------------------------------------------#
      dbhconow        = mymont$DBH
      # dbhcut is the DBH interval in which the cohorts fall
      dbhcut          = cut(dbhconow,breaks=breakdbh)
      # dbhlevs are the possible DBH intervals
      dbhlevs         = levels(dbhcut)
      # dbhfac is the cohort's interval index; if there are 4 possible intervals,
      # depending on the DBH class the cohort will have 1,2,3 or 4 as its interval index
      dbhfac          = match(dbhcut,dbhlevs)
      #---------------------------------------------------------------------------------#



      #----- Define the previous DBH class (for recruitment). --------------------------#
      dbhconow.lastmon = mymont$DBH * exp(-pmax(0,mymont$DLNDBH_DT/12))
      #---------------------------------------------------------------------------------#


      #----- Define the age classes. ---------------------------------------------------#
      ageconow          = rep(x=agepa,times=ncohorts)
      #---------------------------------------------------------------------------------#



      #----- Read the cohort level variables. ------------------------------------------#
      nplantconow       = mymont$NPLANT
      heightconow       = mymont$HITE
      baconow           = mymont$BA_CO
      agbconow          = mymont$AGB_CO
      laiconow          = mymont$MMEAN_LAI_CO
      gppconow          = mymont$MMEAN_GPP_CO
      nppconow          = mymont$MMEAN_NPP_CO
      agb.growthconow   = pmax(0,mymont$DLNAGB_DT)

      #---------------------------------------------------------------------------------#
      #     Find biomass of all tissues.                                                #
      #---------------------------------------------------------------------------------#
      bdeadconow        = mymont$BDEAD
      bleafconow        = mymont$MMEAN_BLEAF_CO
      bsapwoodconow     = mymont$BSAPWOODA+mymont$BSAPWOODB
      if (all(mymont$MMEAN_BROOT_CO == 0)){
        bfrootconow    = ( dbh2bl(dbh=dbhconow.lastmon,ipft=pftconow)
                           * pft$qroot[pftconow] )
      }else{
        bfrootconow    = mymont$MMEAN_BROOT_CO
      }#end if
      bcrootconow       = mymont$BSAPWOODB + (1. - pft$agf.bs[pftconow]) * bdeadconow
      bstemconow        = mymont$BSAPWOODA +       pft$agf.bs[pftconow]  * bdeadconow
      brootconow        = bfrootconow + bcrootconow
      baliveconow       = bleafconow + bfrootconow + bsapwoodconow
      bstorageconow     = mymont$MMEAN_BSTORAGE_CO
      bseedsconow       = mymont$BSEEDS_CO
      biomassconow      = baliveconow + bstorageconow + bseedsconow + bdeadconow
      #---------------------------------------------------------------------------------#

      #---------------------------------------------------------------------------------#
      #      Find the fractions that go to each pool.                                   #
      #---------------------------------------------------------------------------------#
      fg.leaf  = bleafconow  / ( baliveconow + bdeadconow )
      fg.stem  = bstemconow  / ( baliveconow + bdeadconow )
      fg.froot = bfrootconow / ( baliveconow + bdeadconow )
      fg.croot = bcrootconow / ( baliveconow + bdeadconow )
      fs.stem  = bstemconow  / ( bcrootconow + bstemconow )
      fs.croot = bcrootconow / ( bcrootconow + bstemconow )
      #---------------------------------------------------------------------------------#

      #----- Allocation and productivity relative to the total living biomass. ---------#
      f.gppconow        =  100. * gppconow        / pmax(baliveconow,0.01)
      f.nppconow        =  100. * nppconow        / pmax(baliveconow,0.01)
      f.bleafconow      =         bleafconow      / pmax(baliveconow,0.01)
      f.bstemconow      =         bstemconow      / pmax(baliveconow,0.01)
      f.brootconow      =         brootconow      / pmax(baliveconow,0.01)
      #---------------------------------------------------------------------------------#


    }else{
      #----- Make everything NA. -------------------------------------------------------#
      ipaconow            = NA
      icoconow            = NA
      areaconow           = NA
      ageconow            = NA
      pftconow            = NA
      nplantconow         = NA
      heightconow         = NA
      baconow             = NA
      agbconow            = NA
      agb.growthconow     = NA
      laiconow            = NA
      gppconow            = NA
      baliveconow         = NA
      bdeadconow          = NA
      bleafconow          = NA
      bsapwoodconow       = NA
      brootconow          = NA
      bstemconow          = NA
    }#end if
    #------------------------------------------------------------------------------------#

    #------------------------------------------------------------------------------------#
    #      Build the cohort-level lists if this is the right month.                      #
    #------------------------------------------------------------------------------------#
    if (thismonth %in% sasmonth){
      clab = paste( "y",sprintf("%4.4i",thisyear )
                    , "m",sprintf("%2.2i",thismonth),sep="")
      #----- Binding the current cohorts. ----------------------------------------------#
      cohort$ipa          [[clab]] = ipaconow
      cohort$ico          [[clab]] = icoconow
      cohort$area         [[clab]] = areaconow
      cohort$dbh          [[clab]] = dbhconow
      cohort$age          [[clab]] = ageconow
      cohort$pft          [[clab]] = pftconow
      cohort$nplant       [[clab]] = nplantconow * areaconow
      cohort$height       [[clab]] = heightconow
      cohort$ba           [[clab]] = nplantconow * baconow * areaconow
      cohort$agb          [[clab]] = agbconow
      cohort$agb.growth   [[clab]] = 100. * agb.growthconow
      cohort$lai          [[clab]] = laiconow
      cohort$gpp          [[clab]] = gppconow
      cohort$npp          [[clab]] = nppconow
      cohort$balive       [[clab]] = baliveconow
      cohort$bdead        [[clab]] = bdeadconow
      cohort$bleaf        [[clab]] = bleafconow
      cohort$bstem        [[clab]] = bstemconow
      cohort$broot        [[clab]] = brootconow
      cohort$bsapwood     [[clab]] = bsapwoodconow
      cohort$f.bleaf      [[clab]] = f.bleafconow
      cohort$f.bstem      [[clab]] = f.bstemconow
      cohort$f.broot      [[clab]] = f.brootconow
    } #end if month=sasmonth
    #------------------------------------------------------------------------------------#

    #------------------------------------------------------------------------------------#
    #     The following two variables are used to scale "intensive" properties           #
    # (whatever/plant) to "extensive" (whatever/m2).  Sometimes they may be used to      #
    # build weighted averages.                                                           #
    #------------------------------------------------------------------------------------#
    w.nplant  = nplantconow  * areaconow
    w.lai     = laiconow     * areaconow
    w.balive  = baliveconow  * w.nplant
    w.basarea = baconow      * w.nplant
    #------------------------------------------------------------------------------------#


    #------------------------------------------------------------------------------------#
    #     Build the size (DBH) structure arrays.                                         #
    #------------------------------------------------------------------------------------#
    for (d in sequence(ndbh+1)){
      #----- Decide which DBH to use. --------------------------------------------------#
      if (all(is.na(dbhfac))){
        sel.dbh       = rep(FALSE,times=length(dbhfac     ))
        #----- Define the minimum DBH. ------------------------------------------------#
        dbhminconow   = rep(Inf,times=length(pftconow))
        #------------------------------------------------------------------------------#
      }else{
        sel.dbh       = dbhfac      == d | d == (ndbh+1)
        #----- Define the minimum DBH. ------------------------------------------------#
        dbhminconow   = pft$dbh.min[pftconow] * (d == 1) + census.dbh.min * (d != 1)
        #------------------------------------------------------------------------------#
      }#end if
      #---------------------------------------------------------------------------------#


      #----- Decide which PFT to use. --------------------------------------------------#
      for (p in sequence(npft+1)){
        sel.pft   = pftconow == p | p == (npft+1)
        sel       = sel.pft & sel.dbh
        if (any(sel)){
          #----- Extensive properties. -----------------------------------------------#
          szpft$lai         [m,d,p] = sum( laiconow          [sel]
                                           * areaconow         [sel]
                                           , na.rm = TRUE
          )#end if
          szpft$nplant      [m,d,p] = sum( nplantconow       [sel]
                                           * areaconow         [sel]
                                           , na.rm = TRUE
          )#end if
          #----- Intensive properties, use nplant to make them extensive. ------------#
          szpft$agb         [m,d,p] = sum( w.nplant          [sel]
                                           * agbconow          [sel]
                                           , na.rm = TRUE
          )#end if
          szpft$biomass     [m,d,p] = sum( w.nplant          [sel]
                                           * biomassconow      [sel]
                                           , na.rm = TRUE
          )#end if
          szpft$ba          [m,d,p] = sum( w.nplant          [sel]
                                           * baconow           [sel]
                                           , na.rm = TRUE
          )#end if
          szpft$gpp         [m,d,p] = sum( w.nplant          [sel]
                                           * gppconow          [sel]
                                           , na.rm = TRUE
          )#end if
          szpft$npp         [m,d,p] = sum( w.nplant          [sel]
                                           * nppconow          [sel]
                                           , na.rm = TRUE
          )#end if
          szpft$bdead       [m,d,p] = sum( w.nplant          [sel]
                                           * bdeadconow        [sel]
                                           , na.rm = TRUE
          )#end if
          szpft$balive      [m,d,p] = sum( w.nplant          [sel]
                                           * baliveconow       [sel]
                                           , na.rm = TRUE
          )#end if
          szpft$bleaf       [m,d,p] = sum( w.nplant          [sel]
                                           * bleafconow        [sel]
                                           , na.rm = TRUE
          )#end if
          szpft$bstem       [m,d,p] = sum( w.nplant          [sel]
                                           * bstemconow        [sel]
                                           , na.rm = TRUE
          )#end if
          szpft$broot       [m,d,p] = sum( w.nplant          [sel]
                                           * brootconow        [sel]
                                           , na.rm = TRUE
          )#end if
          szpft$bsapwood    [m,d,p] = sum( w.nplant          [sel]
                                           * bsapwoodconow     [sel]
                                           , na.rm = TRUE
          )#end if
        }#end if
        sel = sel.pft & sel.dbh# & dbhconow >= dbhminconow
        if (any(sel)){
          
          acc.growth = sum( w.nplant[sel]
                            * agbconow[sel] * (1.-exp(-agb.growthconow[sel]))
          )#end sum
          szpft$acc.growth [m,d,p] = acc.growth
        }
          #---------------------------------------------------------------------------#
          


        #------------------------------------------------------------------------------#
        # Fractional biomass: use the total variable and divide by the total biomass   #
        #                     so it gives a full community fraction.  Like in the UE   #
        #                     case, force values to be NA if no cohort matches this    #
        #                     class, or if it didn't have much living biomass.         #
        #------------------------------------------------------------------------------#
        balive.szpft              = szpft$balive[m,d,p]
        balive.szpft              = ifelse(balive.szpft > 1.e-7,balive.szpft,NA)
        szpft$f.bleaf     [m,d,p] =        szpft$bleaf     [m,d,p] / balive.szpft
        szpft$f.bstem     [m,d,p] =        szpft$bstem     [m,d,p] / balive.szpft
        szpft$f.broot     [m,d,p] =        szpft$broot     [m,d,p] / balive.szpft
        #------------------------------------------------------------------------------#

      }#end for PFT
      #---------------------------------------------------------------------------------#
    }#end for DBH
    #------------------------------------------------------------------------------------#



    #------------------------------------------------------------------------------------#
    #       Build the derived variables.                                                 #
    #------------------------------------------------------------------------------------#
    emean$nplant          [m] = szpft$nplant         [m,ndbh+1,npft+1]
    emean$lai             [m] = szpft$lai            [m,ndbh+1,npft+1]
    emean$agb             [m] = szpft$agb            [m,ndbh+1,npft+1]
    emean$biomass         [m] = szpft$biomass        [m,ndbh+1,npft+1]
    emean$npp             [m] = szpft$npp            [m,ndbh+1,npft+1]
    #------------------------------------------------------------------------------------#


    H5close()

  }# end for (m in tresume,ntimes)
  #---------------------------------------------------------------------------------------#


  #---------------------------------------------------------------------------------------#
  #     Copy the variables back to datum.                                                 #
  #---------------------------------------------------------------------------------------#
  datum$emean  = emean
  datum$szpft  = szpft
  datum$patch  = patch
  datum$cohort = cohort
  #---------------------------------------------------------------------------------------#

  return(datum)
  #---------------------------------------------------------------------------------------#
}#end function read.q.files
#==========================================================================================#
#==========================================================================================#
