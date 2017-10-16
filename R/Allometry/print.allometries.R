#==========================================================================================#
#==========================================================================================#
#     Leave these commands at the beginning.  They will refresh the session.               #
#------------------------------------------------------------------------------------------#
rm(list=ls())
graphics.off()

#==========================================================================================#
#      Here is the user defined variable section.                                          #
#==========================================================================================#


#------------------------------------------------------------------------------------------#
#                                  Paths                                                   #
#------------------------------------------------------------------------------------------#
out.dir  = "/Users/manfredo/Desktop/r_minimal/ED2io/R/Allometry/"
here     = "/Users/manfredo/Desktop/r_minimal/ED2io/R"
setwd(here)

library(ggplot2)
library(NCmisc)
source("allometry.r")
source("operators.r")
source("rconstants.r")
source("pft.coms.r")
source("unitlist.r")
source("pmonthly.varlist.r")

#------------------------------------------------------------------------------------------#
#                   Retrieve allometric function list                                      #
#------------------------------------------------------------------------------------------#

is_function = function (expr) { # is this a function?
  #if (! is_assign(expr))
  if (! is.call(expr) && as.character(expr[[1]]) %in% c('=', '<-', '<<-', 'assign'))
    return(FALSE)
  value = expr[[3]]
  is.call(value) && as.character(value[[1]]) == 'function'
}

function_name = function (expr)
  as.character(expr[[2]])

is_assign = function (expr)
  is.call(expr) && as.character(expr[[1]]) %in% c('=', '<-', '<<-', 'assign')

file_parsed = parse("allometry.R")
functions = Filter(is_function, file_parsed)
function_names = unlist(Map(function_name, functions))
#------------------------------------------------------------------------------------------#

pftuse = c(2,3,4,17)
no.plot = c("dbh2bl.alt", "agb.SL", "agb2dbh.baker", "dbh2agb.baker")

xlimit                  = list()
xlimit[["dbh"   ]]      = c(tiny.num,22 )
xlimit[["agb"   ]]      = c(tiny.num,100)
xlimit[["h"     ]]      = c(tiny.num,35 )

ycomp                   = list()
ycomp[["crownbh"]]$desc = "Crown Height"
ycomp[["crownbh"]]$unit = untab$m
ycomp[["dbh"    ]]$desc = "DBH"
ycomp[["dbh"    ]]$unit = untab$cm
ycomp[["agb"    ]]$desc = "Above Ground Biomass"
ycomp[["agb"    ]]$unit = untab$kgcom2
ycomp[["wai"    ]]$desc = "Wood Area Index"
ycomp[["wai"    ]]$unit = untab$m2wom2
ycomp[["vol"    ]]$desc = "Volume"
ycomp[["vol"    ]]$unit = untab$m3
ycomp[["rd"     ]]$desc = "Rooting Depth"
ycomp[["rd"     ]]$unit = untab$m
ycomp[["ca"     ]]$desc = "Canopy Area"
ycomp[["ca"     ]]$unit = untab$m2
ycomp[["bl"     ]]$desc = "Leaf Biomass"
ycomp[["bl"     ]]$unit = untab$kgcom2
ycomp[["bd"     ]]$desc = "Structural Biomass"
ycomp[["bd"     ]]$unit = untab$kgcom2
ycomp[["h"      ]]$desc = "Height"
ycomp[["h"      ]]$unit = untab$m

for (f in function_names){
  #f="h2dbh"
  if (f %in% no.plot) next
  
  myf       = get(f)
  myyout    = gsub(pattern=".*2",replacement="",f)
  #g         = rep(seq_along(tspftdbh),sapply(tspftdbh,length))
  #yindex    = g[match(myyout,unlist(tspftdbh))]
  myxin     = names(formals(myf))[1]
  myxlim    = xlimit[[myxin]]
  flowlimit = min(myf(myxlim[1],pftuse))
  fuplimit  = max(myf(myxlim[2],pftuse))
  myylim    = c(flowlimit,fuplimit)
  
  fichier   = file.path(out.dir, paste(f,".pdf",sep=""))
  pdf(file=fichier, onefile = F,width = 6, height = 4)
  #x11()
  
  #par(mar=c(4,4.5,4,2)+0.1)
  par(mar=c(4,4.5,2,2)+0.1)
  plot.new()
  plot.window(xlim=myxlim, ylim=myylim,asp=0.3)
  axis(side=1, lwd=2)
  axis(side=2,las=1, lwd=2)
  box(lwd=2)
  
  ley = desc.unit(ycomp[[myyout]]$desc, ycomp[[myyout]]$unit)
  lex = desc.unit(myxin,ycomp[[myxin]]$unit)
  mtext(text = lex , side = 1, outer= T, line = -1.5, cex = 1.5)
  mtext(text = ley , side = 2, outer= T, line = -2.0, cex = 1.5)
  
  
  title(main=paste(ycomp[[myyout]]$desc, "as a function of", myxin), lwd = 1)
  legend (myxlim[1],myylim[2], pft$name[pftuse], lwd = 2, col=pft$colour[pftuse])
  
  for (p in pftuse){
    
    curve (myf(h=x, dbh=x, agb = x, ipft = p), 0, myxlim[2],
           col=pft$colour[p], add=T, lwd = 2)
    
  }
  dev.off()
  
}

#locator(n=1)
#dev.off()

