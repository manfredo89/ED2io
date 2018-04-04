#==========================================================================================#
#==========================================================================================#
#     Leave these commands at the beginning.  They will refresh the session.               #
#------------------------------------------------------------------------------------------#
rm(list=ls())
graphics.off()
#==========================================================================================#
#==========================================================================================#

library(ggplot2)

source ("/Users/manfredo/Desktop/r_minimal/ED2io/R/unitlist.r")
source ("/Users/manfredo/Desktop/r_minimal/ED2io/R/rconstants.r")
source ("/Users/manfredo/Desktop/r_minimal/ED2io/R/allometry.R")
source ("/Users/manfredo/Desktop/r_minimal/ED2io/R/operators.r")
source ("/Users/manfredo/Desktop/r_minimal/ED2io/R/pft.coms.r")

where = "/Users/manfredo/Desktop/r_minimal/ED2io/R/Allometry/parameter/"

ssfact = 3.0

pftuse = c(2,3,4,17)
pftnames = c("Early","Mid","Late","Liana")


para=list()

para[[1]]=list(
  name = "vm0",
  description = "Maximum Photosynthetic capacity at 25 °C",
  unit = untab$umolom2os,
  value = c(18.75,12.5,6.25,18.75) * ssfact * vmfact.c3
)


para[[2]]=list(
  name = "mort3",
  description = "Ageing mortality (density independent)",
  unit = untab$oneoyr,
  value = c(0.07235222,0.04156404,0.0,0.06311576)
)

para[[3]]=list(
  name = "leaf_turnover_rate",
  description = "Leaf / fine root turnover rate",
  unit = untab$oneoyr,
  value = c(1.0,0.5,1./3,1.27)
)

para[[4]]=list(
  name = "treefall_s_gtht",
  description = "Tall plant (>10m) surviving a nearby treefall",
  unit = untab$pc,
  value = c(0,0,0,0.9)
)

para[[5]]=list(
  name = "rho",
  description = "Wood Density",
  unit = untab$gocm3,
  value = c(0.4,0.6,0.87,0.46)
)

para[[6]]=list(
  name = "sla",
  description = "Specific Leaf Area",
  unit = "m[l*e*a*f]^{2}*K*g[C]^phantom(1)^{-1}",
  value =  pft$SLA[pftuse]
)


for (i in seqle(para)){
  
  filename = paste(where,para[[i]]$name, ".pdf",sep = "")
  pdf(filename)
  par(mar=c(5.1,4.5,4.1,2.1))
  my.ylab = desc.unit(desc = para[[i]]$name, unit = para[[i]]$unit)
  p = barplot(para[[i]]$value, col=pft$colour[pftuse], mgp=c(2.5,1,.5),
                 main = para[[i]]$description, xlab = "PFT", ylab = my.ylab,
                 names.arg = pftnames, cex.names = 1.5, cex.axis = 1.5,cex.main=1.6, cex.lab=1.5)
 # axis(2,cex.axis=1.5)
  mylab = round(para[[i]]$value, digits = 2)
  text(p,  y = mylab ,labels=mylab,cex = 2, pos = 1, offset=2.5)
  dev.off()
  

  
}


