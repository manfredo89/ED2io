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
  szpft  = datum$szpft
  patch  = datum$patch
  dbhds  = datum$dbhds
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
    patch$agb          [[plab]] = array(data=0. ,dim=c(mymont$NPATCHES_GLOBAL, npftuse))
    patch$bleaf        [[plab]] = array(data=0. ,dim=c(mymont$NPATCHES_GLOBAL, npftuse))
    patch$lai          [[plab]] = array(data=0. ,dim=c(mymont$NPATCHES_GLOBAL, npftuse))
    patch$gpp          [[plab]] = array(data=0. ,dim=c(mymont$NPATCHES_GLOBAL, npftuse))
    patch$nplant       [[plab]] = array(data=0. ,dim=c(mymont$NPATCHES_GLOBAL, npftuse))
    
    #------------------------------------------------------------------------------------#
    #     Read the cohort-level variables.  Because empty patchs do exist (deserts),     #
    # we must check whether there is any cohort to be read.  If not, assign NA to        #
    # all variables.                                                                     #
    #------------------------------------------------------------------------------------#
    one.cohort = sum(ncohorts) == 1
    one.patch  = sum(npatches) == 1
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
      
      #------------------------------------------------------------------------------------#
      #     The following two variables are used to scale "intensive" properties           #
      # (whatever/plant) to "extensive" (whatever/m2).  Sometimes they may be used to      #
      # build weighted averages.                                                           #
      #------------------------------------------------------------------------------------#
      w.nplant  = nplantconow  * areaconow
      #------------------------------------------------------------------------------------#
      
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
      #      Find total NPP then total autotrophic, growth and storage respiration.     #
      # The latter two will be distributed amongst tissues.                             #
      #---------------------------------------------------------------------------------#
      #----- Monthly means. ------------------------------------------------------------#
      gppconow          = mymont$MMEAN_GPP_CO
      nppconow          = mymont$MMEAN_NPP_CO
      npprconow         = mymont$MMEAN_NPPFROOT_CO + mymont$MMEAN_NPPCROOT_CO
      nppsconow         = mymont$MMEAN_NPPSAPWOOD_CO
      npplconow         = mymont$MMEAN_NPPLEAF_CO
      nppwconow         = mymont$MMEAN_NPPWOOD_CO
      nppxconow         = mymont$MMEAN_NPPSEEDS_CO
      #---------------------------------------------------------------------------------#
      #      Find the fractions that go to each pool.                                   #
      #---------------------------------------------------------------------------------#
      fg.leaf    = bleafconow  / ( baliveconow + bdeadconow )
      fg.stem    = bstemconow  / ( baliveconow + bdeadconow )
      fg.froot   = bfrootconow / ( baliveconow + bdeadconow )
      fg.croot   = bcrootconow / ( baliveconow + bdeadconow )
      fs.stem    = bstemconow  / ( bcrootconow + bstemconow )
      fs.croot   = bcrootconow / ( bcrootconow + bstemconow )
      
      
      #---------------------------------------------------------------------------------#
      #      Attribute respiration to the different pools.  Assuming that non-          #
      # structural carbon respiration
      #---------------------------------------------------------------------------------#
      #----- Mean monthly cycle. ----------------------------------------------------#
      leaf.respconow  = ( mymont$MMEAN_LEAF_RESP_CO
                          + mymont$MMEAN_LEAF_GROWTH_RESP_CO
                          + mymont$MMEAN_LEAF_STORAGE_RESP_CO
      )#end leaf.respconow
      stem.respconow  = ( mymont$MMEAN_SAPA_GROWTH_RESP_CO
                          + mymont$MMEAN_SAPA_STORAGE_RESP_CO
      )#end leaf.respconow
      froot.respconow = ( mymont$MMEAN_ROOT_RESP_CO
                          + mymont$MMEAN_ROOT_GROWTH_RESP_CO
                          + mymont$MMEAN_ROOT_STORAGE_RESP_CO
      )#end leaf.respconow
      croot.respconow = ( mymont$MMEAN_SAPB_GROWTH_RESP_CO
                          + mymont$MMEAN_SAPB_STORAGE_RESP_CO
      )#end leaf.respconow
      root.respconow  = froot.respconow + croot.respconow
      #---------------------------------------------------------------------------------#
      
      
      
      
      ###################################################################################
      ################ Here I have finished reading the H5 file #########################
      ###################################################################################
      
      
      #----- Allocation and productivity relative to the total living biomass. ---------#
      f.gppconow        =  100. * gppconow        / pmax(baliveconow,0.01)
      f.nppconow        =  100. * nppconow        / pmax(baliveconow,0.01)
      f.bstorageconow   =         bstorageconow   / pmax(baliveconow,0.01)
      f.bleafconow      =         bleafconow      / pmax(baliveconow,0.01)
      f.bstemconow      =         bstemconow      / pmax(baliveconow,0.01)
      f.brootconow      =         brootconow      / pmax(baliveconow,0.01)
      f.bseedsconow     =         bseedsconow     / pmax(baliveconow,0.01)
      #---------------------------------------------------------------------------------#
      
      
      #------ Find the demographic rates. ----------------------------------------------#
      if (one.cohort){
        mortconow    = sum(mymont$MMEAN_MORT_RATE_CO)
        mortconow    = max(0,mortconow)
      }else{
        mortconow    = try(colSums(mymont$MMEAN_MORT_RATE_CO))
        if ("try-error" %in% is(mortconow)) browser()
        mortconow    = pmax(0,mortconow)
      }#end if
      ncbmortconow    = pmax(0,mymont$MMEAN_MORT_RATE_CO[2,])
      dimortconow     = pmax(0,mortconow - ncbmortconow)
      growthconow     = pmax(0,mymont$DLNDBH_DT)
      agb.growthconow = pmax(0,mymont$DLNAGB_DT)
      lcostconow        = mymont$MMEAN_LEAF_MAINTENANCE_CO * yr.day
      
      
      #------ Find the AGB and basal area of the previous month. -----------------------#
      agbcolmon        = agbconow * exp(-agb.growthconow/12.)
      #---------------------------------------------------------------------------------#
      
      
      
      #----- Find the variables that must be rendered extensive. -----------------------#
      
      maxh.pa   = (data.frame(tapply(X= heightconow,           INDEX = list(ipaconow, pftconow),
                                     FUN = max)))[sequence(npftuse)]
      agb.pa    = (data.frame(tapply(X= agbconow * w.nplant,   INDEX = list(ipaconow, pftconow),
                                     FUN = sum)))[sequence(npftuse)]
      bleaf.pa  = (data.frame(tapply(X= bleafconow * w.nplant, INDEX = list(ipaconow, pftconow),
                                     FUN = sum)))[sequence(npftuse)]
      lai.pa    = (data.frame(tapply(X= laiconow * areaconow,  INDEX = list(ipaconow, pftconow),
                                     FUN = sum)))[sequence(npftuse)]
      gpp.pa    = (data.frame(tapply(X= gppconow * w.nplant,   INDEX = list(ipaconow, pftconow),
                                     FUN = sum)))[sequence(npftuse)]
      nplant.pa = (data.frame(tapply(X= nplantconow,           INDEX = list(ipaconow, pftconow),
                                     FUN = sum)))[sequence(npftuse)]
      
      #---------------------------------------------------------------------------------#
      
      #---------------------------------------------------------------------------------#
      #     Copy the data back to the patch.                                            #
      #---------------------------------------------------------------------------------#
      patch$maxh   [[plab]] = maxh.pa
      patch$agb    [[plab]] = agb.pa
      patch$bleaf  [[plab]] = bleaf.pa
      patch$lai    [[plab]] = lai.pa
      patch$gpp    [[plab]] = gpp.pa
      patch$nplant [[plab]] = nplant.pa
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
      
      laiconow            = NA
      gppconow            = NA
      leaf.respconow      = NA
      stem.respconow      = NA
      root.respconow      = NA
      nppconow            = NA
      npprconow           = NA
      nppsconow           = NA
      npplconow           = NA
      nppxconow           = NA
      nppwconow           = NA
      lcostconow          = NA
      baliveconow         = NA
      bdeadconow          = NA
      bleafconow          = NA
      bsapwoodconow       = NA
      brootconow          = NA
      bstemconow          = NA
      bstorageconow       = NA
      bseedsconow         = NA
      mortconow           = NA
      ncbmortconow        = NA
      dimortconow         = NA
      agb.growthconow     = NA
      
      f.bstorageconow     = NA
      f.bleafconow        = NA
      f.bstemconow        = NA
      f.brootconow        = NA
      f.bseedsconow       = NA
    }#end if
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
        dbhminconow   = pft$dbh.min[pftconow]
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
          szpft$nppr        [m,d,p] = sum( w.nplant          [sel]
                                           * npprconow         [sel]
                                           , na.rm = TRUE
          )#end if
          szpft$npps        [m,d,p] = sum( w.nplant          [sel]
                                           * nppsconow        [sel]
                                           , na.rm = TRUE
          )#end if
          szpft$nppl        [m,d,p] = sum( w.nplant          [sel]
                                           * npplconow        [sel]
                                           , na.rm = TRUE
          )#end if
          szpft$nppx        [m,d,p] = sum( w.nplant          [sel]
                                           * nppxconow         [sel]
                                           , na.rm = TRUE
          )#end if
          szpft$nppw        [m,d,p] = sum( w.nplant          [sel]
                                           * nppwconow         [sel]
                                           , na.rm = TRUE
          )#end if
          szpft$lco          [m,d,p] = sum( w.nplant          [sel]
                                            * lcostconow        [sel]
                                            , na.rm = TRUE
          )#end sum
          szpft$leaf.resp    [m,d,p] = sum( w.nplant          [sel]
                                            * leaf.respconow    [sel]
                                            , na.rm = TRUE
          )#end sum
          szpft$stem.resp    [m,d,p] = sum( w.nplant          [sel]
                                            * stem.respconow    [sel]
                                            , na.rm = TRUE
          )#end sum
          szpft$root.resp    [m,d,p] = sum( w.nplant          [sel]
                                            * root.respconow    [sel]
                                            , na.rm = TRUE
          )#end sum
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
          szpft$bseeds       [m,d,p] = sum( w.nplant          [sel]
                                            * bseedsconow       [sel]
                                            , na.rm = TRUE
          )#end sum
          szpft$bstorage     [m,d,p] = sum( w.nplant          [sel]
                                            * bstorageconow     [sel]
                                            , na.rm = TRUE
          )#end sum
          
        }
        #---------------------------------------------------------------------------#
        
        #------------------------------------------------------------------------------#
        # Fractional biomass: use the total variable and divide by the total biomass   #
        #                     so it gives a full community fraction.  Like in the UE   #
        #                     case, force values to be NA if no cohort matches this    #
        #                     class, or if it didn't have much living biomass.         #
        #------------------------------------------------------------------------------#
        b.szpft              = szpft$balive[m,d,p] + szpft$bstorage[m,d,p] +
          + szpft$bdead[m,d,p]
        b.szpft              = ifelse(b.szpft > 1.e-7,b.szpft,NA)
        szpft$f.bstorage  [m,d,p] =        szpft$bstorage  [m,d,p] / b.szpft
        szpft$f.bleaf     [m,d,p] =        szpft$bleaf     [m,d,p] / b.szpft
        szpft$f.bstem     [m,d,p] =        szpft$bstem     [m,d,p] / b.szpft
        szpft$f.broot     [m,d,p] =        szpft$broot     [m,d,p] / b.szpft
        szpft$f.bseeds    [m,d,p] =        szpft$bseeds    [m,d,p] / b.szpft
        #------------------------------------------------------------------------------#
        
        
        #------------------------------------------------------------------------------#
        #    For mortality and growth, we keep deleting the tiny guys because they     #
        # skew the rates quite significantly.                                          #
        #------------------------------------------------------------------------------#
        sel = sel.pft & sel.dbh & dbhconow >= dbhminconow
        if (any(sel)){
          
          acc.growth = sum( w.nplant[sel]
                            * agbconow[sel] * (1.-exp(-agb.growthconow[sel]))
          )#end sum
          szpft$acc.growth [m,d,p] = acc.growth
          #---------------------------------------------------------------------------#
          #      Find the total number of plants and previous population if the only  #
          # mortality was the mortality we test.                                      #
          #---------------------------------------------------------------------------#
          survivor             = sum( w.nplant[sel]                          )
          previous             = sum( w.nplant[sel] * exp(mortconow   [sel]) )
          ncb.previous         = sum( w.nplant[sel] * exp(ncbmortconow[sel]) )
          di.previous          = sum( w.nplant[sel] * exp(dimortconow [sel]) )
          szpft$mort   [m,d,p] = log( previous     / survivor )
          szpft$ncbmort[m,d,p] = log( ncb.previous / survivor )
          szpft$dimort [m,d,p] = log( di.previous  / survivor )
          #---------------------------------------------------------------------------#
          
          
          
          #---------------------------------------------------------------------------#
          #      Find the total AGB and previous AGB if the only mortality was the    #
          # mortality we test.                                                        #
          #---------------------------------------------------------------------------#
          survivor                 = sum( w.nplant[sel] * agbcolmon[sel])
          previous                 = sum( w.nplant[sel] * agbcolmon[sel]
                                          * exp(mortconow            [sel] / 12.) )
          ncb.previous             = sum( w.nplant[sel] * agbcolmon[sel]
                                          * exp(ncbmortconow         [sel] / 12. ) )
          di.previous              = sum( w.nplant[sel] * agbcolmon[sel]
                                          * exp(dimortconow          [sel] / 12. ) )
          szpft$acc.mort   [m,d,p] = 12. * (previous     - survivor)
          szpft$acc.ncbmort[m,d,p] = 12. * (ncb.previous - survivor)
          szpft$acc.dimort [m,d,p] = 12. * (di.previous  - survivor)
          #---------------------------------------------------------------------------#
          
          
        }
      }#end for PFT
      #---------------------------------------------------------------------------------#
    }#end for DBH
    #------------------------------------------------------------------------------------#
    
    #Temporary variables for debugging purpose. These can go once everyting is set
    print.sizehisto = T
    if(print.sizehisto && any (ncohorts > 0)){
      
      for (i in names(dbhds)){
        
        if (i == "tree_clss"){
          this.classdbh   = c(0,10,15,20,25,30,35,40,50,60,70,80,90,100)
        } else {
          this.classdbh   = c(0,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,20)
        }
        
        this.breakdbh = c(-Inf,this.classdbh[-1],Inf)
        this.ndbh     = length(this.classdbh)
        # dbhcut is the DBH interval in which the cohorts fall
        this.dbhcut   = cut(dbhconow,breaks=this.breakdbh)
        # dbhlevs are the possible DBH intervals
        this.dbhlevs  = levels(this.dbhcut)
        # dbhfac is the cohort's interval index; if there are 4 possible intervals,
        # depending on the DBH class the cohort will have 1,2,3 or 4 as its interval index
        this.dbhfac   = match(this.dbhcut,this.dbhlevs)
        #---------------------------------------------------------------------------------#
        
        #---------------------------------------------------------------------------------#
        #     Build the size (DBH) structure arrays.                                      #
        #---------------------------------------------------------------------------------#
        for (d in sequence(this.ndbh+1)){
          #----- Decide which DBH to use. ------------------------------------------------#
          if (all(is.na(this.dbhfac))){
            sel.dbh = rep(FALSE,times=length(this.dbhfac))
          }else{
            sel.dbh = this.dbhfac == d | d == (this.ndbh+1)
          }#end if
          #-------------------------------------------------------------------------------#
          
          
          #----- Decide which PFT to use. ------------------------------------------------#
          for (p in sequence(npft+1)){
            sel.pft   = pftconow == p | p == (npft+1)
            sel       = sel.pft & sel.dbh
            if (any(sel)){
              
              dbhds[[i]][m,d,p] = sum( nplantconow [sel]* areaconow [sel], na.rm = TRUE)
            }
            #-----------------------------------------------------------------------------#
            
          }#end for PFT
          #-------------------------------------------------------------------------------#
        }#end for DBH
        #---------------------------------------------------------------------------------#
      }#end for idbh
      #-----------------------------------------------------------------------------------#
    }#end if sizehisto
    
    H5close()
    
  }# end for (m in tresume,ntimes)
  #---------------------------------------------------------------------------------------#
  
  #---------------------------------------------------------------------------------------#
  #     Copy the variables back to datum.                                                 #
  #---------------------------------------------------------------------------------------#
  datum$szpft  = szpft
  datum$patch  = patch
  datum$dbhds  = dbhds
  #---------------------------------------------------------------------------------------#
  
  return(datum)
  #---------------------------------------------------------------------------------------#
}#end function read.q.files
#==========================================================================================#
#==========================================================================================#
