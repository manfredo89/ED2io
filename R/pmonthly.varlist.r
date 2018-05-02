#------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------#
#     List of possible plots. In case you don't want some of them, simply switch plt to F. #
#------------------------------------------------------------------------------------------#
#----- Time series per PFT. ---------------------------------------------------------------#
tspftdbh          = list()
n                 = 1
tspftdbh[[n]]     = list( vnam     = "agb"
                          , desc     = "Above ground biomass"
                          , e.unit   = untab$kgcom2
                          , i.unit   = untab$kgcopl
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , bar.plot = TRUE
                          , stack    = TRUE
                          , scsout   = TRUE
)#end list 1
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "biomass"
                          , desc     = "Total biomass"
                          , e.unit   = untab$kgcom2
                          , i.unit   = untab$kgcopl
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , bar.plot = TRUE
                          , stack    = TRUE
                          , scsout   = TRUE
)#end list 2
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "ba"
                          , desc     = "Basal area"
                          , e.unit   = untab$cm2om2
                          , i.unit   = untab$cm2opl
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , bar.plot = TRUE
                          , stack    = TRUE
                          , scsout   = TRUE
)#end list 3
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "lai"
                          , desc     = "Leaf area index"
                          , e.unit   = untab$m2lom2
                          , i.unit   = untab$m2lom2
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , bar.plot = TRUE
                          , stack    = TRUE
                          , scsout   = TRUE
)#end list 4
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "gpp"
                          , desc     = "Gross primary productivity"
                          , e.unit   = untab$kgcom2oyr
                          , i.unit   = untab$kgcom2opl
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , bar.plot = TRUE
                          , stack    = TRUE
                          , scsout   = TRUE
)#end list 5
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "npp"
                          , desc     = "Net primary productivity"
                          , e.unit   = untab$kgcom2oyr
                          , i.unit   = untab$kgcoployr
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = FALSE
)#end list 6
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "nppr"
                          , desc     = "Root Net primary productivity"
                          , e.unit   = untab$kgcom2oyr
                          , i.unit   = untab$kgcoployr
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = FALSE
)#end list 6
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "npps"
                          , desc     = "Sapwood Net primary productivity"
                          , e.unit   = untab$kgcom2oyr
                          , i.unit   = untab$kgcoployr
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = FALSE
)#end list 6
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "nppl"
                          , desc     = "Leaf Net primary productivity"
                          , e.unit   = untab$kgcom2oyr
                          , i.unit   = untab$kgcoployr
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = FALSE
)#end list 6
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "nppx"
                          , desc     = "Seed Net primary productivity"
                          , e.unit   = untab$kgcom2oyr
                          , i.unit   = untab$kgcoployr
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = FALSE
)#end list 6
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "nppw"
                          , desc     = "Wood Net primary productivity"
                          , e.unit   = untab$kgcom2oyr
                          , i.unit   = untab$kgcoployr
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = FALSE
)#end list 6
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "lco"
                          , desc     = "Leaf litter"
                          , e.unit   = untab$kgcom2oyr
                          , i.unit   = untab$kgcoployr
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , sas      = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = TRUE
)#end list
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "balive"
                          , desc     = "Biomass of active tissues"
                          , e.unit   = untab$kgcom2
                          , i.unit   = untab$kgcopl
                          , plog     = FALSE
                          , pft      = FALSE
                          , pftdbh   = FALSE
                          , bar.plot = FALSE
                          , stack    = TRUE
                          , scsout   = FALSE
)#end list 7
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "bdead"
                          , desc     = "Structural biomass"
                          , e.unit   = untab$kgcom2
                          , i.unit   = untab$kgcopl
                          , plog     = FALSE
                          , pft      = FALSE
                          , pftdbh   = FALSE
                          , bar.plot = FALSE
                          , stack    = TRUE
                          , scsout   = TRUE
)#end list 8
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "bleaf"
                          , desc     = "Leaf biomass"
                          , e.unit   = untab$kgcom2
                          , i.unit   = untab$kgcopl
                          , plog     = FALSE
                          , pft      = FALSE
                          , pftdbh   = FALSE
                          , bar.plot = FALSE
                          , stack    = TRUE
                          , scsout   = TRUE
)#end list 9
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "broot"
                          , desc     = "Root biomass"
                          , e.unit   = untab$kgcom2
                          , i.unit   = untab$kgcopl
                          , plog     = FALSE
                          , pft      = FALSE
                          , pftdbh   = FALSE
                          , bar.plot = FALSE
                          , stack    = TRUE
                          , scsout   = TRUE
)#end list 10
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "bstem"
                          , desc     = "Stem biomass"
                          , e.unit   = untab$kgcom2
                          , i.unit   = untab$kgcopl
                          , plog     = FALSE
                          , pft      = FALSE
                          , pftdbh   = FALSE
                          , bar.plot = FALSE
                          , stack    = TRUE
                          , scsout   = TRUE
)#end list 11
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "bsapwood"
                          , desc     = "Sapwood biomass"
                          , e.unit   = untab$kgcom2
                          , i.unit   = untab$kgcopl
                          , plog     = FALSE
                          , pft      = FALSE
                          , pftdbh   = FALSE
                          , bar.plot = FALSE
                          , stack    = TRUE
                          , scsout   = TRUE
)#end list 12
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "bseeds"
                          , desc     = "Seed biomass"
                          , e.unit   = untab$kgcom2
                          , i.unit   = untab$kgcopl
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , sas      = FALSE
                          , bar.plot = FALSE
                          , stack    = TRUE
                          , scsout   = TRUE
)#end list
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "bstorage"
                          , desc     = "Storage biomass"
                          , e.unit   = untab$kgcom2
                          , i.unit   = untab$kgcopl
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , bar.plot = FALSE
                          , stack    = TRUE
                          , scsout   = TRUE
)#end list 13
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "mort"
                          , desc     = "Mortality rate"
                          , e.unit   = untab$pcpopoyr
                          , i.unit   = untab$pcpopoyr
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , sas      = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = FALSE
)#end list
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "ncbmort"
                          , desc     = "Mortality rate - Neg. C balance"
                          , e.unit   = untab$pcpopoyr
                          , i.unit   = untab$pcpopoyr
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , sas      = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = TRUE
)#end list
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "dimort"
                          , desc     = "Mortality rate - Density independent"
                          , e.unit   = untab$pcpopoyr
                          , i.unit   = untab$pcpopoyr
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , sas      = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = TRUE
)#end list
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "acc.growth"
                          , desc     = "Growth rate"
                          , e.unit   = untab$kgcom2oyr
                          , i.unit   = untab$kgcoployr
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , sas      = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = TRUE
)#end list 15
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "acc.mort"
                          , desc     = "Mortality rate"
                          , e.unit   = untab$kgcom2oyr
                          , i.unit   = untab$kgcoployr
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , sas      = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = FALSE
)#end list
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "acc.ncbmort"
                          , desc     = "Mortality rate - Neg. C balance"
                          , e.unit   = untab$kgcom2oyr
                          , i.unit   = untab$kgcoployr
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , sas      = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = TRUE
)#end list
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "acc.dimort"
                          , desc     = "Mortality rate - Density independent"
                          , e.unit   = untab$kgcom2oyr
                          , i.unit   = untab$kgcoployr
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , sas      = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = TRUE
)#end list
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "acc.recr"
                          , desc     = "Recruitment rate"
                          , e.unit   = untab$kgcom2oyr
                          , i.unit   = untab$kgcoployr
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , sas      = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = TRUE
)#end list
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "nplant"
                          , desc     = "Plant density"
                          , e.unit   = untab$plom2
                          , i.unit   = untab$plom2
                          , plog     = TRUE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , bar.plot = TRUE
                          , stack    = TRUE
                          , scsout   = TRUE
)#end list 14
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "f.bstorage"
                          , desc     = "Relative storage biomass"
                          , e.unit   = untab$kgcokgc
                          , i.unit   = untab$kgcokgc
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , sas      = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = TRUE
)#end list
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "f.bleaf"
                          , desc     = "Relative leaf biomass"
                          , e.unit   = untab$kgcokgc
                          , i.unit   = untab$kgcokgc
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , sas      = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = TRUE
)#end list
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "f.bstem"
                          , desc     = "Relative stem biomass"
                          , e.unit   = untab$kgcokgc
                          , i.unit   = untab$kgcokgc
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , sas      = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = TRUE
)#end list
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "f.broot"
                          , desc     = "Relative root biomass"
                          , e.unit   = untab$kgcokgc
                          , i.unit   = untab$kgcokgc
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , sas      = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = TRUE
)#end list
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "f.bseeds"
                          , desc     = "Relative seed biomass"
                          , e.unit   = untab$kgcokgc
                          , i.unit   = untab$kgcokgc
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , sas      = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = TRUE
)#end list
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "leaf.resp"
                          , desc     = "Leaf respiration"
                          , e.unit   = untab$kgcom2oyr
                          , i.unit   = untab$kgcoployr
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , sas      = FALSE
                          , bar.plot = FALSE
                          , stack    = TRUE
                          , scsout   = TRUE
)#end list
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "stem.resp"
                          , desc     = "Stem respiration"
                          , e.unit   = untab$kgcom2oyr
                          , i.unit   = untab$kgcoployr
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , sas      = FALSE
                          , bar.plot = FALSE
                          , stack    = TRUE
                          , scsout   = TRUE
)#end list
n                 = n + 1
tspftdbh[[n]]     = list( vnam     = "root.resp"
                          , desc     = "Root respiration"
                          , e.unit   = untab$kgcom2oyr
                          , i.unit   = untab$kgcoployr
                          , plog     = FALSE
                          , pft      = TRUE
                          , pftdbh   = TRUE
                          , sas      = FALSE
                          , bar.plot = FALSE
                          , stack    = TRUE
                          , scsout   = TRUE
)#end list

####################################################################################
####################################################################################
####################################################################################
####################################################################################

patch_plots       = list()
n                 = 1
patch_plots[[n]]  = list( vnam     = "maxh"
                          , desc     = "Maximum height"
                          , e.unit   = untab$m
                          , i.unit   = untab$m
                          , plog     = FALSE
                          , pft      = FALSE
                          , pftdbh   = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = FALSE
)#end list 1
n=n+1
patch_plots[[n]]  = list( vnam     = "agb"
                          , desc     = "Above Ground Biomass"
                          , e.unit   = untab$kgcom2
                          , i.unit   = untab$kgcopl
                          , plog     = FALSE
                          , pft      = FALSE
                          , pftdbh   = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = FALSE
)#end list 2
n=n+1
patch_plots[[n]]  = list( vnam     = "bleaf"
                          , desc     = "Leaf Biomass"
                          , e.unit   = untab$kgcom2
                          , i.unit   = untab$kgcopl
                          , plog     = FALSE
                          , pft      = FALSE
                          , pftdbh   = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = FALSE
)#end list 2
n=n+1
patch_plots[[n]]  = list( vnam     = "lai"
                          , desc     = "Leaf Area Index"
                          , e.unit   = untab$m2lom2
                          , i.unit   = untab$m2lom2
                          , plog     = FALSE
                          , pft      = FALSE
                          , pftdbh   = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = FALSE
)#end list 4
n=n+1
patch_plots[[n]]  = list( vnam     = "gpp"
                          , desc     = "Gross Primary productivity"
                          , e.unit   = untab$kgcom2oyr
                          , i.unit   = untab$kgcom2opl
                          , plog     = FALSE
                          , pft      = FALSE
                          , pftdbh   = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = FALSE
)#end list 5
n=n+1
patch_plots[[n]]  = list( vnam     = "nplant"
                          , desc     = "Plant density"
                          , e.unit   = untab$plom2
                          , i.unit   = untab$plom2
                          , plog     = FALSE
                          , pft      = FALSE
                          , pftdbh   = FALSE
                          , bar.plot = FALSE
                          , stack    = FALSE
                          , scsout   = FALSE
)#end list 1

#------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------#
#     Define the number of plots of each kind, and make the lists global.                  #
#------------------------------------------------------------------------------------------#
tserdist     <<- TRUE                  # Time series of disturbance rates
ntspftdbh    <<- length(tspftdbh)
npatch_plots <<- length(patch_plots)
#------------------------------------------------------------------------------------------#
