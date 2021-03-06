#==========================================================================================#
#==========================================================================================#
#Manfredo: h[tropn] should be modified to h * tropn and similarly to all the other maks
#arguments order have now a certain importance, for instance if one passes multiple values
#for a specific input the output will respect the order so h2dbh(h=c(5,8), ipft=c(1,2,17))
#will ouput as h2dbh(5,1) h2dbh(8,1) h2dbh(5,2) h2dbh(8,2) h2dbh(5,17) h2dbh(8,17)
# similarly for all other functions
#----- Safe logical operators.  These will always return FALSE if x or y are not finite. --#
#' Height to dbh allometry
#' @rdname h2dbh
#' @usage h2dbh(h,ipft)
#' @description This is an allometric function to return the dbh as a function of height
#' @param h height
#' @param ipft PFT
#' @return dbh diameter at breast height
#' @export
#'
h2dbh <- function(h,ipft,...){

  if(length(ipft) != length(h)) {
    zpft = rep(ipft, each=length(h))
    zh = rep(h, length(ipft))
  }else{
    zpft = ipft
    zh = h
  }

  dbh   = NA * h
  tropo = pft$tropical[zpft] & iallom %in% c(0,1)
  tropn = pft$tropical[zpft] & iallom %in% c(2,3)
  tempe = ! pft$tropical[zpft]

  dbh[tropo] = exp((log(zh*tropo)-pft$b1Ht[zpft[tropo]])/pft$b2Ht[zpft[tropo]])
  dbh[tropn] = ( log( pft$hgt.ref[zpft[tropn]] / ( pft$hgt.ref[zpft[tropn]] - zh*tropn ) )
                 / pft$b1Ht[zpft[tropn]] ) ^ ( 1. / pft$b2Ht[zpft[tropn]])
  dbh[tempe] = log( 1.0 - ( zh*tempe - pft$hgt.ref[zpft[tempe]])
                    / pft$b1Ht[zpft[tempe]] ) / pft$b2Ht[zpft[tempe]]

  return(dbh)
}#end function h2dbh

#==========================================================================================#

#' dbh to height allometry
#' @rdname dbh2h
#' @usage dbh2h(dbh,ipft)
#' @description This is an allometric function to return the height as a function of dbh
#' @param dbh diameter at breast height
#' @param ipft PFT
#' @return h height
#' @export

dbh2h <- function(dbh,ipft,...){

  if(length(ipft) != length(dbh)){
    zpft = rep(ipft, each=length(dbh))
    zdbh = rep(dbh, length(ipft))
  }else{
    zpft = ipft
    zdbh = dbh
  }

  dbhuse        = zdbh
  large         = is.finite(zdbh) & zdbh > pft$dbh.crit[zpft]
  dbhuse[large] = pft$dbh.crit[zpft * large]

  tropo         = pft$tropical[zpft] & iallom %in% c(0,1)
  tropn         = pft$tropical[zpft] & iallom %in% c(2,3)
  tempe         = ! pft$tropical[zpft]

  h         = NA * dbh
  h[tropo]  = exp(pft$b1Ht[zpft[tropo]] + pft$b2Ht[zpft[tropo]] * log(dbhuse[tropo]) )
  h[tropn]  = ( pft$hgt.ref[zpft[tropn]]
                * (1.0 - exp( -pft$b1Ht[zpft[tropn]] * dbhuse^pft$b2Ht[zpft[tropn]] ) ) )
  h[tempe]  = ( pft$hgt.ref[zpft[tempe]] + pft$b1Ht[zpft[tempe]]
                * (1.0 - exp(pft$b2Ht[zpft[tempe]] * dbhuse[tempe] ) ) )

  return(h)
}#end function dbh2h

#==========================================================================================#

#' Dbh to leaf biomass allometry
#' @rdname dbh2bl
#' @usage dbh2bl(dbh,ipft)
#' @description This is an allometric function to return the leaf
#' biomass as a function of dbh
#' @param dbh diameter at breast height
#' @param ipft PFT
#' @return bleaf leaf biomass
#' @export

dbh2bl <- function(dbh,ipft,...){
 #browser()
  if(length(ipft) != length(dbh)){
    zpft = rep(ipft, each=length(dbh))
    zdbh = rep(dbh, length(ipft))
  }else{
    zpft = ipft
    zdbh = dbh
  }

  is.liana = pft$liana[zpft]
  dbhuse = pmin(zdbh,pft$dbh.crit[zpft])
  bleaf  = ifelse( dbhuse %<% pft$dbh.adult[zpft]
                   , pft$b1Bl.small[zpft] /C2B * dbhuse ^ pft$b2Bl.small[zpft]
                   , ifelse(is.liana, pft$b1Bl.large[zpft] /C2B * dbhuse ^ pft$b2Bl.large[zpft] - 0.1538
                                    , pft$b1Bl.large[zpft] /C2B * dbhuse ^ pft$b2Bl.large[zpft]
                   )
  )#end ifelse

  return(bleaf)
}# end function dbh2bl

#==========================================================================================#

#' dbh to dead biomass allometry
#' @name dbh2bd
#' @usage dbh2bd(dbh,ipft)
#' @description This is an allometric function to return the dead biomass
#' as a function of dbh
#' @param dbh diameter at breast height
#' @param ipft PFT
#' @return bdead dead biomass
#' @export

dbh2bd <- function(dbh,ipft,...){
  if(length(ipft) != length(dbh)){
    zpft = rep(ipft, each=length(dbh))
    zdbh = rep(dbh, length(ipft))
  }else{
    zpft = ipft
    zdbh = dbh
  }

  small = is.finite(zdbh) & zdbh <= pft$dbh.crit[zpft]
  large = is.finite(zdbh) & zdbh >  pft$dbh.crit[zpft]
  #is.liana = pft$liana[zpft]

  bdead = NA * zdbh
  bdead[small] = ( pft$b1Bs.small[zpft[small]] / C2B * zdbh[small]
                   ^ pft$b2Bs.small[zpft[small]] )
  bdead[large] = ( pft$b1Bs.large[zpft[large]] / C2B * zdbh[large]
                   ^ pft$b2Bs.large[zpft[large]] )
  #bdead[liana] =  10. ^ 0.12 * (0.785 * zdbh * zdbh) ^ 0.91
  #bdead[is.liana] =  0.13745 * (zdbh[is.liana]) ^ 2.69373
  return(bdead)
}# end function dbh2bl

#==========================================================================================#

#' dbh to canopy area allometry
#' @name dbh2ca
#' @usage dbh2ca(dbh,ipft)
#' @description This is an allometric function to return the canopy area,
#' it is using Dietze and Clark allometry(2008)
#' @param dbh diameter at breast height
#' @param ipft PFT
#' @return crown canopy area
#' @export

dbh2ca <- function(dbh,ipft,...){

  if(length(ipft) != length(dbh)){
    zpft = rep(ipft, each=length(dbh))
    zdbh = rep(dbh, length(ipft))
  }else{
    zpft = ipft
    zdbh = dbh
  }

  #----- Find local LAI, the minimum size for a crown area. ------------------------------#
  bleaf  = dbh2bl(dbh,ipft)
  loclai = pft$SLA[zpft] * bleaf

  dbhuse        = zdbh
  large         = is.finite(zdbh) & zdbh > pft$dbh.crit[zpft]
  dbhuse[large] = pft$dbh.crit[zpft[large]]
  crown         = pft$b1Ca[zpft] * dbhuse ^ pft$b2Ca[zpft]

  #----- Local LAI / Crown area should never be less than one. ---------------------------#
  crown = pmin(crown,loclai)

  return(crown)
}#end function dbh2ca

#==========================================================================================#

#' dbh to wood area index allometry
#' @name dbh2wai
#' @usage dbh2wai(dbh,ipft)
#' @description This is an allometric function to return the wood area index
#' it is derived from Ahrends et al. (2010).
#' @param dbh diameter at breast height
#' @param ipft PFT
#' @param chambers Use Chambers method?(set to FALSE)
#' @return wai wood area index
#' @export

dbh2wai <- function(dbh,ipft,chambers=FALSE,...){

  if(length(ipft) != length(dbh)){
    zpft = rep(ipft, each=length(dbh))
    zdbh = rep(dbh, length(ipft))
  }else{
    zpft = ipft
    zdbh = dbh
  }

  dbh.use  = pmin(pft$dbh.crit[zpft],zdbh)
  #---------------------------------------------------------------------------------------#
  #     Chambers method.                                                                  #
  #---------------------------------------------------------------------------------------#
  if (chambers){
    h        = dbh2h(ipft=zpft,dbh=dbh.use)
    wdens    = ifelse(is.na(pft$rho[zpft]),0.6,pft$rho[zpft])
    hcb      = h2crownbh(h=h,ipft=zpft)
    dcb      = 1.045676 * dbh/hcb^0.091
    dcb.use  = 1.045676 * dbh.use/hcb^0.091
    abole    = 2.5e-3 * pi * (dbh.use+dcb.use) * sqrt(4*hcb^2-1.e-4*(dbh.use-dcb.use)^2)
    vbole    = 1.0e-4 * pi * hcb * (dbh.use^2+dbh.use*dcb.use+dcb.use^2) / 12.
    bbole    = 1000. * wdens * vbole / C2B
    bleaf    = dbh2bl(dbh=dbh.use,ipft=zpft)
    bsapwood = bleaf * pft$qsw[zpft] * h
    bdead    = dbh2bd(dbh=dbh.use,ipft=zpft)
    agb.wood = pft$agf.bs[zpft] * (bsapwood + bdead)
    bbranch  = agb.wood - bbole
    dbmin    = 0.2 + 0 * dbh.use
    kterm    = 0.4 * bbranch*C2B/(pi*wdens*(dcb.use-dbmin))
    abranch  = pi*kterm*log(dcb.use/dbmin)
    wai      = ( abole + abranch )
    wai      = wai * dbh2ca(dbh=pft$dbh.crit[zpft],ipft=zpft)/max(wai)
  }else if(! iallom %in% c(3)){
    wai      = pft$b1WAI[zpft] * dbh.use ^ pft$b2WAI[zpft]
  }else{
    wai      = 0.11 * pft$SLA[zpft] * dbh2bl(dbh=dbh.use,ipft)
  }#end if
  if (any(is.na(wai))) browser()
  #---------------------------------------------------------------------------------------#


  return(wai)
}#end function dbh2ca

#==========================================================================================#

#' dbh to volume allometry
#' @name dbh2vol
#' @usage dbh2vol(dbh,ipft)
#' @description This is an allometric function to return the volume
#' @param dbh diameter at breast height
#' @param ipft PFT
#' @return vol volume
#' @export

dbh2vol <- function(dbh,ipft,...,hgt){

  if(length(ipft) != length(dbh)){
    zpft = rep(ipft, each=length(dbh))
    zdbh = rep(dbh, length(ipft))
  }else{
    zpft = ipft
    zdbh = dbh
  }

  if (missing(hgt)) hgt = dbh2h (dbh, ipft)

  vol  = pft$b1Vol[zpft] * hgt * zdbh ^ pft$b2Vol[zpft]
  return(vol)
}#end function dbh2ca
#==========================================================================================#
#==========================================================================================#


#==========================================================================================#
#==========================================================================================#
#    Rooting depth.                                                                        #
#------------------------------------------------------------------------------------------#
dbh2rd <- function(dbh,ipft,...,hgt){

  if(length(ipft) != length(dbh)){
    zpft = rep(ipft, each=length(dbh))
    zdbh = rep(dbh, length(ipft))
  }

  if (missing(hgt)) hgt = dbh2h (dbh, ipft)

  if (iallom %in% c(0)){
    #------------------------------------------------------------------------------------#
    #    Original ED-2.1 (I don't know the source for this equation, though).            #
    #------------------------------------------------------------------------------------#
    vol  = dbh2vol(dbh,ipft,hgt)
    rd   = pft$b1Rd[ipft] * vol ^ pft$b2Rd[ipft]
  }else if (iallom %in% c(1,2,3)){
    #-----------------------------------------------------------------------------------#
    #    This is just a test allometry, that imposes root depth to be 0.5 m for         #
    # plants that are 0.15-m tall, and 5.0 m for plants that are 35-m tall.             #
    #-----------------------------------------------------------------------------------#
    rd = pft$b1Rd[zpft] * hgt ^ pft$b2Rd[zpft]
    #-----------------------------------------------------------------------------------#
  }#end if
  return(rd)
}#end function dbh2rd
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#    This function finds the trunk height.  Currently this is based on the following       #
# reference, which is for a site in Bolivia:                                               #
#                                                                                          #
# Poorter L., L. Bongers, F. Bongers, 2006: Architecture of 54 moist-forest tree           #
#     species: traits, trade-offs, and functional groups. Ecology, 87, 1289-1301.          #
#------------------------------------------------------------------------------------------#
h2crownbh <- function (h,ipft,...){

  if(length(ipft) != length(h)){
    zpft = rep(ipft, each=length(h))
    zh = rep(h, length(ipft))
  }else{
    zpft = ipft
    zh = h
  }

  crown.length = pft$b1Cl[zpft] * zh ^ pft$b2Cl[zpft]
  ans          = pmax(0.05,zh - crown.length)
  return(ans)
}#end function h2crownbh
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#    This function finds the leaf biomass for different plants.  This is based on the      #
# following papers:                                                                        #
#                                                                                          #
#  Cole, T. J., J. J. Ewel, 2006:  Allometric equations for four valuable tropical tree    #
#      species.  Forest Ecol. Management, 229, 351-360.                                    #
#                                                                                          #
#  Calvo-Alvarado, J. C., N. G. McDowell, R. H. Waring, 2008:  Allometric relationships    #
#      predicting foliar biomass and leaf area:sapwood area ratio from tree height in five #
#      Costa Rican rain forest species.  Tree Physiol. 28, 1601-1608.                      #
#------------------------------------------------------------------------------------------#
dbh2bl.alt <- function (dbh,genus,...){
  #----- Make genus case insensitive. ----------------------------------------------------#
  genushere = tolower(genus)
  #---------------------------------------------------------------------------------------#


  #---------------------------------------------------------------------------------------#
  #     Decide which equation to use based on the genus, or if it is to call ED-2.1, just #
  # call the good old dbh2bl...                                                           #
  #---------------------------------------------------------------------------------------#
  if (genushere == "cedrela"){
    h = dbh2h(3,dbh)
    x = dbh^2 * h

    small = is.finite(dbh) & dbh <= 10.
    large = is.finite(dbh) & dbh >  10.

    bleaf = NA * dbh
    bleaf[small] = 0.1265 / C2B * x[small] ^ 0.2787
    bleaf[large] = 0.0013 / C2B * x[large] ^ 0.9218

  }else if(genushere == "cordia"){
    h = dbh2h(2,dbh)
    x = dbh^2 * h

    small = is.finite(dbh) & dbh <= 5.
    large = is.finite(dbh) & dbh >  5.

    bleaf = NA * dbh
    bleaf[small] = 0.3041 / C2B * x[small] ^ 0.1082
    bleaf[large] = 0.0391 / C2B * x[large] ^ 0.5151


  }else if(genushere == "hyeronima"){
    h = dbh2h(4,dbh)
    x = dbh^2 * h

    small = is.finite(dbh) & dbh <= 10.
    large = is.finite(dbh) & dbh >  10.

    bleaf = NA * dbh
    bleaf[small] = 0.2144 / C2B * x[small] ^ 0.2852
    bleaf[large] = 0.0094 / C2B * x[large] ^ 0.6910

  }else if(genushere == "tetragastris"){
    bleaf = 0.040 / C2B * dbh ^ 1.737

  }else if(genushere == "virola"){
    bleaf = 0.002 / C2B * dbh ^ 2.468

  }else if(genushere == "carapa"){
    bleaf = 0.012 / C2B * dbh ^ 2.089

  }else if(genushere == "vochysia"){
    bleaf = 0.673 / C2B * dbh ^ 1.058

  }else if(genushere == "pentaclethra"){
    bleaf = 0.958 / C2B * dbh ^ 0.757

  }else if(genushere == "grass"){
    bleaf = dbh2bl(dbh,1)

  }else if(genushere == "early"){
    bleaf = dbh2bl(dbh,2)

  }else if(genushere == "mid"){
    bleaf = dbh2bl(dbh,3)

  }else if(genushere == "late"){
    bleaf = dbh2bl(dbh,4)

  }else{
    stop (paste("Genus ",genus," wasn't found.  ",
                "Sorry, I can't find bleaf for this one...",sep=""))
  }#end if
  return(bleaf)
}#end function h2crownbh
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#     We find the above-ground biomass based on dbh and wood density, following the        #
# allometry proposed by:                                                                   #
#                                                                                          #
# Baker, T. R., and co-authors, 2004: Variation in wood density determines spatial         #
#     patterns in Amazonian forest biomass. Global Change Biol., 10, 545-562.              #
#                                                                                          #
# Chave, J., B. Riera, M.A. Dubois, 2001: Estimation of biomass in a neotropical forest of #
#     French Guiana: spatial and temporal variability.  J. Trop. Ecol., 17, 79-96.         #
#------------------------------------------------------------------------------------------#
dbh2agb.baker <- function(dbh,wdens,allom="baker.chave",...){


  ln.dbh = log(dbh)


  if (allom == "baker.chave"){
    #------ Use Chave's based function (equation 2, Baker et al., 2004). ----------------#
    agb = wdens / 0.58 / C2B * exp(2.42 * ln.dbh - 2.00)
    #------------------------------------------------------------------------------------#
  }else if (allom == "baker.chambers"){
    #------ Use Chambers' function. -----------------------------------------------------#
    agb = ( wdens / 0.67 / C2B
            * exp(0.33 * ln.dbh + 0.933 * ln.dbh^2 - 0.122 * ln.dbh^3 - 0.37) )
    #------------------------------------------------------------------------------------#
  }else if (allom == "chave.2006"){
    #------ Use Chambers' function. -----------------------------------------------------#
    agb = ( wdens / C2B
            * exp( -1.499 + 2.1481 * ln.dbh + 0.207 * ln.dbh^2 - 0.0281 * ln.dbh^3 ) )
    #------------------------------------------------------------------------------------#
  }#end if
  #---------------------------------------------------------------------------------------#
  return(agb)
}#end if
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#     We find the dbh based on above-ground biomass and wood density, following the        #
# allometry proposed by:                                                                   #
#                                                                                          #
# Baker, T. R., and co-authors, 2004: Variation in wood density determines spatial         #
#     patterns in Amazonian forest biomass. Global Change Biol., 10, 545-562.              #
#                                                                                          #
# Chave, J., B. Riera, M.A. Dubois, 2001: Estimation of biomass in a neotropical forest of #
#     French Guiana: spatial and temporal variability.  J. Trop. Ecol., 17, 79-96.         #
#------------------------------------------------------------------------------------------#
agb2dbh.baker <- function(agb,wdens,allom="baker.chave",...){


  ln.agb  = log(agb)
  ln.rhon = log(wdens / 0.58 / C2B)


  if (allom == "baker.chave"){
    #------ Use Chave's based function (equation 2, Baker et al., 2004). ----------------#
    dbh = exp( 1 / 2.42 * ( ln.agb - ln.rhon + 2.00 ) )
    #------------------------------------------------------------------------------------#
  }else if (allom == "baker.chambers"){
    #------ Use Chambers' function. -----------------------------------------------------#
    stop("Cannot invert Chambers' equation...")
    #------------------------------------------------------------------------------------#
  }else if (allom == "chave.2006"){
    #------ Use Chambers' function. -----------------------------------------------------#
    stop ("Cannot invert Chave 2006 equation...")
    #------------------------------------------------------------------------------------#
  }#end if
  #---------------------------------------------------------------------------------------#
  return(dbh)
}#end if
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#     Biomass allometry that is used by Sustainable Landscapes.  Results are always in     #
# kgC/plant.                                                                               #
#                                                                                          #
# References:                                                                              #
#                                                                                          #
# Chave, J., and co-authors, 2014: Improved allometric models to estimate tha aboveground  #
#     biomass of tropical trees.  Glob. Change Biol., 20, 3177-3190                        #
#     doi:10.1111/gcb.12629                                                                #
#                                                                                          #
# Goodman, R., and co-authors, 2013: Amazon palm biomass and allometry.  Forest Ecol.      #
#     Manag., 310, 994-1004. doi:10.1016/j.foreco.2013.09.045                              #
#                                                                                          #
# Palace, M., and co-authors, 2007: Necromass in undisturbed ad logged forests in the      #
#     Brazilian Amazon.  Forest Ecol. Manag., 238, 309-318.                                #
#     doi:10.1016/j.foreco.2006.10.026                                                     #
#                                                                                          #
# Schnitzer, S. A., and co-authors, 2006: Censusing and measuring lianas: a quantitative   #
#     comparison of the common methods.  Biotropica, 38, 581-591                           #
#     doi:10.1111/j.1744-7429.2006.00187.x                                                 #
#                                                                                          #
#------------------------------------------------------------------------------------------#
agb.SL <- function(dbh,h,wdens,type=NULL,dead=NULL,...){
  #---------------------------------------------------------------------------------------#
  #     "type" and "dead" may not be present, in which case we use dummy values.          #
  #---------------------------------------------------------------------------------------#
  if (is.null(type)) type = rep(  "O",times=length(dbh))
  if (is.null(dead)) dead = rep(FALSE,times=length(dbh))
  #---------------------------------------------------------------------------------------#



  #---------------------------------------------------------------------------------------#
  #     Make sure all terms have the same length.                                         #
  #---------------------------------------------------------------------------------------#
  lens = unique(c(length(dbh),length(h),length(wdens),length(type),length(dead)))
  if ( length(lens) != 1 ){
    cat("-----------------------------------------------------------","\n",sep="")
    cat("   Variables don't have the same length."                   ,"\n",sep="")
    cat("   DBH    = ",length(dbh)                                   ,"\n",sep="")
    cat("   HEIGHT = ",length(h)                                     ,"\n",sep="")
    cat("   WDENS  = ",length(wdens)                                 ,"\n",sep="")
    cat("   TYPE   = ",length(type)                                  ,"\n",sep="")
    cat("   DEAD   = ",length(dead)                                  ,"\n",sep="")
    cat("-----------------------------------------------------------","\n",sep="")
    stop(" Incorrect input data.")
  }else{
    fine.dbh    = is.numeric  (dbh)    || all(is.na(dbh   ))
    fine.h      = is.numeric  (h)      || all(is.na(h))
    fine.wdens  = is.numeric  (wdens)  || all(is.na(wdens ))
    fine.type   = is.character(type)   || all(is.na(type  ))
    fine.dead   = is.logical  (dead)   || all(is.na(dead  ))
    if (! all(c(fine.dbh,fine.h,fine.wdens,fine.type,fine.dead))){
      cat("-----------------------------------------------------------","\n",sep="")
      cat("   Not all variables have the correct type."                ,"\n",sep="")
      cat("   DBH    (numeric)   = ",fine.dbh                          ,"\n",sep="")
      cat("   HEIGHT (numeric)   = ",fine.h                            ,"\n",sep="")
      cat("   WDENS  (numeric)   = ",fine.wdens                        ,"\n",sep="")
      cat("   TYPE   (character) = ",fine.type                         ,"\n",sep="")
      cat("   DEAD   (logical)   = ",fine.dead                         ,"\n",sep="")
      cat("-----------------------------------------------------------","\n",sep="")
      stop(" Incorrect data types.")
    }#end if (! all(c(fine.dbh,fine.h,fine.wdens,fine.type,fine.dead)))
  }#end if ( length(lens) != 1)
  #---------------------------------------------------------------------------------------#
  
  #----- Initialise the output. ----------------------------------------------------------#
  agb = NA * dbh
  #---------------------------------------------------------------------------------------#
  
  #---------------------------------------------------------------------------------------#
  #     Find the possible statuses, then choose the best equation.                        #
  #---------------------------------------------------------------------------------------#
  tree  = type %in% "O" & (! dead)
  palm  = type %in% "P"
  liana = type %in% "L"
  dead  = type %in% "O" & dead
  #---------------------------------------------------------------------------------------#
  
  #---------------------------------------------------------------------------------------#
  #     AGB by type.                                                                      #
  #---------------------------------------------------------------------------------------#
  #----- Living tree: Chave et al. (2014). -----------------------------------------------#
  agb[tree ] = 0.0673 * (wdens[tree]*dbh[tree]^2*h[tree])^0.976 / C2B
  #----- Palm: Goodman et al. (2013). ----------------------------------------------------#
  agb[palm ] = exp(-3.448+0.588/2) * dbh[palm]^2.7483 / C2B
  #----- Liana: Schnitzer et al. (2006). -------------------------------------------------#
  agb[liana] = exp(-0.968) * dbh[liana]^2.657 / C2B
  #----- Dead trees: Palace et al. (2007). -----------------------------------------------#
  v1 = 0.091
  v0 = 0.01 / (1.3^-v1)
  a0 = 0.25 * pi * v0^2 / (1. - 2*v1)
  a1 = 1 - 2*v1
  agb[dead] = 1000. * wdens[dead] * a0 * dbh[dead]^2 * h[dead]^a1 / C2B
  #---------------------------------------------------------------------------------------#
  
  
  return(agb)
}#end function agb.SL
#==========================================================================================#
#==========================================================================================#
