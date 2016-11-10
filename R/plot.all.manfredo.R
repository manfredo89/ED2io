#==========================================================================================#
#' @title Main plot utility
#' @author Marcos Longo & Manfredo di Porcia
#'
#'
#'
#'
#==========================================================================================#
#     Leave these commands at the beginning.  They will refresh the session.               #
#------------------------------------------------------------------------------------------#
rm(list=ls())
graphics.off()


#------------------------------------------------------------------------------------------#
#              Load the necessary packages                                                 #
#------------------------------------------------------------------------------------------#

library (rhdf5)
library (chron)
library (reshape2)
library (ggplot2)


#==========================================================================================#
#      Here is the user defined variable section.                                          #
#==========================================================================================#


#------------------------------------------------------------------------------------------#
#                                  Paths                                                   #
#------------------------------------------------------------------------------------------#
# For now a maximum of two entries is supported, please don't enter more
# To increase, modify: yeara
site.dir    = "/Users/manfredo/Documents/Eclipse_workspace/ED/build/post_process/paracou/"
run.type    = c("nolianas")
here        = "/Users/manfredo/Desktop/r_minimal/ED2io/R"
setwd(here)

#------------------------------------------------------------------------------------------#
#                             Time options                                                 #
#------------------------------------------------------------------------------------------#
reload.data    = TRUE                               # Should I reload partially loaded data?
sasmonth.short = c(2,5,8,11)                        # Months for SAS plots (short runs)
sasmonth.long  = 5                                  # Months for SAS plots (long runs)
nyears.long    = 15                                 # Max number of years for short run

#------------------------------------------------------------------------------------------#
#                             Plot options                                                 #
#------------------------------------------------------------------------------------------#
plt.opt = list()
plt.opt$height  = 6.8          # plot height
plt.opt$width   = 8.8          # plot width
plt.opt$typeleg = length(run.type) > 1

#==========================================================================================#
#      No need to change beyond this point (unless you are developing the code)            #
#==========================================================================================#

#---------------------------------------------------------------------------------------#
#                Source all the needed files with the sourcer function                  #
#---------------------------------------------------------------------------------------#
source("sourcer.R")
sourcer(source.dir = here)
#---------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------#
#                                     Paths                                                #
#------------------------------------------------------------------------------------------#
place            = "paracou"                          # Simulation locus

file.dir         = vector(mode="character", length=length(run.type))
for (i in seqle(run.type)){
  file.dir[i] = paste(site.dir, run.type[i], sep = "")
}

analy.path       = paste(file.dir,"analy",sep='/')  # analysis folder
analy.path.place = paste(analy.path,place,sep='/')   # same with locus prefix
com.list         = list.files(analy.path, pattern = "*-Q-*")
if(length(run.type) > 1)
com.list         = com.list[duplicated(com.list)]


#------------------------------------------------------------------------------------------#
#              Check if the figures directory exists.  If not, create it.                  #
#------------------------------------------------------------------------------------------#
# hey here it's maybe better to select a different way to name the folder, like this is
# not safe and perhaps misleading
if (length(file.dir) <= 1){
  figdir = file.path(file.dir, "figures")
} else {
  dirname = paste0(run.type,sep="_",collapse="")
  figdir = file.path(paste(site.dir,dirname,"comparison",sep=""))
}
dir.make(figdir)
#------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------#
#                     Keep only full years                                                 #
#------------------------------------------------------------------------------------------#
# first year of available data in the folder
yeara            = min(sub('\\-.*','',sub("^.*?Q-","",com.list)))
# last year if available data in the folder
yearz            = max(sub('\\-.*','',sub("^.*?Q-","",com.list)))

#for trials so to shorten things up
#yearz = 2033

yeara = as.integer(yeara)        # convert yeara to factor
yearz = as.integer(yearz)        # convert yearz to factor

if( any(!file.exists(paste(analy.path.place,"-Q-",yeara,"-01-00-000000-g01.h5",sep = ""))))
  yeara = yeara + 1
if( any(!file.exists(paste(analy.path.place,"-Q-",yearz,"-12-00-000000-g01.h5",sep = ""))))
  yearz = yearz - 1

#---------------------------------------------------------------------------------------#
#                    Check time margin consistency                                      #
#---------------------------------------------------------------------------------------#
if (yeara >= yearz){
  cat(" - Yeara:  ",yeara,"\n")
  cat(" - Yearz:  ",yearz,"\n")
  cat(" - Prefix: ",analy.path,"\n")
  cat(" - Invalid years, will not process data...","\n")
  stop()
  }#end if
#---------------------------------------------------------------------------------------#

#----- Decide how frequently the cohort-level variables should be saved. ---------------#
if ((yearz - yeara + 1) <= nyears.long){
  sasmonth   = sasmonth.short
}else{
  sasmonth   = sasmonth.long
}#end if
#---------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------#
#                Set time variables to read the input files                             #
#---------------------------------------------------------------------------------------#
nyears      =  yearz - yeara + 1
ntimes      =  nyears * 12
#---------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------#
#      Make the RData file name, then we check whether we must read the files again     #
#      or use the stored RData.                                                         #
#---------------------------------------------------------------------------------------#
path.data   = file.path(file.dir,"rdata_month")
dir.make(path.data)
ed22.rdata  = file.path(path.data,paste(place,"RData",sep="."))
ed22.status = file.path(path.data,paste("status_",place,".txt",sep=""))

yfac  = vector('integer')
patch = list()
szpft = list()
runn = 1
for (csite in file.dir){
  if (reload.data && file.exists(ed22.rdata[runn])){
    #----- Load the modelled dataset. ---------------------------------------------------#
    cat("   - Loading previous session...","\n")
    load(ed22.rdata[runn])
    tresume = datum$ntimes + 1
    if (ntimes > datum$ntimes){
      datum  = update.monthly( new.ntimes = ntimes
                                     , old.datum   = datum
                                     , yeara      = yeara
                                     , inpref     = analy.path.place[runn]
      )#end update.monthly
    }#end if
    #------------------------------------------------------------------------------------#
  }else{
    cat("   - Starting new session...","\n")
    tresume      = 1
    datum     = create.monthly( ntimes  = ntimes
                                      , yeara   = yeara
                                      , inpref  = analy.path.place[runn]
    )#end create.monthly
  }#end if
#---------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------#
#     Check whether we have anything to update.                                         #
#---------------------------------------------------------------------------------------#
complete = tresume > ntimes
#---------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------#
#     Loop over all times in case there is anything new to be read.                     #
#---------------------------------------------------------------------------------------#
if (! complete){

  #------------------------------------------------------------------------------------#
  #     This function will read the files.                                             #
  #------------------------------------------------------------------------------------#
  datum = read.q.files(datum=datum,ntimes=ntimes,tresume=tresume,sasmonth=sasmonth)
  #------------------------------------------------------------------------------------#

  #------ Save the data to the R object. ----------------------------------------------#
  cat(" + Saving data to ",basename(ed22.rdata[runn]),"...","\n")
  save(datum,file=ed22.rdata[runn])
  #------------------------------------------------------------------------------------#
}#end if (! complete)
#---------------------------------------------------------------------------------------#

#----- Update status file with latest data converted into R. ---------------------------#
latest = paste(datum$year[ntimes],datum$month[ntimes],sep=" ")
dummy  = write(x=latest,file=ed22.status[runn],append=FALSE)
#---------------------------------------------------------------------------------------#

#----- Make some shorter versions of some variables. -----------------------------------#
szpft[[runn]] = datum$szpft
patch[runn] = datum$patch
#yfac   = datum$year
#szpft  = c(szpft,datum$szpft)
#patch  = c(patch,datum$patch)
# mfac    not used
# cohort  not used
#---------------------------------------------------------------------------------------#
# To order data timewise uncomment next command
# View(lapply(cohort, "[[", "y2013m02"))
runn = runn + 1
}
yfac  = datum$year

#=======================================================================================#
#                          Plotting section starts here                                 #
#=======================================================================================#


# All PFTs (including total)
allpft      = which(apply(X=szpft[[1]]$nplant,MARGIN=3,FUN=sum,na.rm=TRUE) > 0.)
# Remove total
pftuse      = allpft[allpft != (npft+1)]
# Number of PFTs in use
npftuse     = length(pftuse)
# PFT colours
mycol       = pft$colour
# PFT names
mynam       = pft$name

for(n in sequence(ntspftdbh - 1)){
  #----- Load settings for this variable.----------------------------------------------#
  thisvar     = tspftdbh[[n]]
  vnam        = thisvar$vnam
  description = thisvar$desc
  unit        = thisvar$e.unit
  plog        = thisvar$plog

  if (! vnam %in% names(szpft[[1]])){next}

  #---------------------------------------------------------------------------------#
  #     Check if the n directory exists.  If not, create it.                        #
  #---------------------------------------------------------------------------------#
  outdir = file.path(figdir,vnam)
  if (! dir.exists(outdir)) dir.create(outdir)
  #---------------------------------------------------------------------------------#

  #---------------------------------------------------------------------------------------#
  #     Remove all elements of the DBH/PFT class that do not have a single valid cohort   #
  # at any given time.                                                                    #
  #---------------------------------------------------------------------------------------#
  df = list()
  for(i in seqle(run.type)){
    dft = szpft[[i]][[vnam]][,ndbh+1,]
    row.names(dft) = yfac
    empty = (is.na(dft) | dft == 0) & !(col(dft) %in% allpft)
    dft[empty] = NA
    dft = reshape2::melt(dft,varnames = c("Year","mpft"), na.rm = T)
    dft$type = i
    dft$index = interaction(dft$mpft,i)
    df = rbind(df,dft)
  }
  #---------------------------------------------------------------------------------------#

  #---------------------- Set y limit and axis label -------------------------------------#
  ylimit = range(df$value)
  y.txt = desc.unit(desc = description, unit = unit)
  #---------------------------------------------------------------------------------------#

  for (y in tail(datum$toyear, n = 10L)){

    dfy = subset(df, df$Year==y)
    y.lab.txt = as.expression(paste(y.txt, y, sep = "-"))

    ggplot(dfy,aes(x=rep(1:12, length(unique(dfy$index))), y=value, colour = factor(mpft))) +
      geom_line(aes(group = index, linetype=LETTERS[dfy$type]), size = .8,
                show.legend = plt.opt$typeleg) +
      geom_point(aes(group = index)) +
      scale_linetype_manual(name= "Run Type", values=lty.stock(seqle(run.type)),labels = run.type) +
      scale_color_manual(name = "PFT",
                         values = setNames(mycol[unique(dfy$mpft)], unique(dfy$mpft)),
                         labels = setNames(mynam[unique(dfy$mpft)], unique(dfy$mpft))) +
      scale_x_continuous(breaks = sequence(12), labels = month.abb) +
      scale_y_continuous(limits = ylimit) +
      ylab(y.txt) +
      xlab("Month") +
      theme(legend.position = "bottom",
            axis.title   = element_text(size = 16),
            axis.text    = element_text(face = "bold", size = 12, colour = "black"),
            plot.title   = element_text(lineheight = .8, face="bold"),
            legend.text  = element_text(size = 14),
            legend.title = element_text(size = 14, face="bold"),
            legend.key.size = unit(2, "lines"),
            legend.background = element_rect(color="black", fill = NA, size=.5, linetype = 1),
            panel.border = element_rect(color = "black", fill = NA, size=0.5)) +
      ggtitle(description)

    file.name = paste(vnam,"-",y,".pdf",sep="")
    ggsave(file.name, plot = last_plot(), device = "pdf", path = outdir,
           width = plt.opt$width, height = plt.opt$height)

  }


}


#---------------------------------------------------------------------------------------#
#     Consolidate the yearly means for the long-term dynamics (the PFT and DBH/PFT      #
# stuff).                                                                               #
#---------------------------------------------------------------------------------------#
cat ("    - Finding the annual statistics for multi-dimensional variables...","\n")
cat ("      * Aggregating the annual mean of PFT-DBH variables...","\n")
for(i in seqle(run.type)){
  for (vname in names(szpft[[i]]))
    szpft[[i]][[vname]] = qapply(X=szpft[[i]][[vname]],INDEX=yfac,DIM=1,FUN=mean,na.rm=TRUE)


  #---------------------------------------------------------------------------------------#
  #     Remove all elements of the DBH/PFT class that do not have a single valid cohort   #
  # at any given time.                                                                    #
  #---------------------------------------------------------------------------------------#
  empty = is.na(szpft[[i]]$nplant) | szpft[[i]]$nplant == 0
  for (vname in names(szpft[[i]])) szpft[[i]][[vname]][empty] = NA
}
#---------------------------------------------------------------------------------------#

#------------------------- Find which PFTs we need to consider -------------------------#
pftave  = apply( X      = szpft[[1]]$agb[,ndbh+1,] #ndbh+1 contains the total
                 , MARGIN = 2
                 , FUN    = mean
                 , na.rm  = TRUE
)#end apply
#---------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#----------------------- total PFT plots -----------------------------------#
#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
if(nyears > 5){
  total.outdir = file.path(figdir,"totals")
  if (! dir.exists(total.outdir)) dir.create(total.outdir)

  for(n in sequence(ntspftdbh - 1)){
    #----- Load settings for this variable.----------------------------------------------#
    thisvar     = tspftdbh[[n]]
    vnam        = thisvar$vnam
    description = thisvar$desc
    unit        = thisvar$e.unit
    plog        = thisvar$plog

    if (! vnam %in% names(szpft[[1]])) next

    #---------------------------------------------------------------------------------#
    #     Check if the n directory exists.  If not, create it.                        #
    #---------------------------------------------------------------------------------#
    outdir = file.path(figdir,vnam)
    if (! dir.exists(outdir)) dir.create(outdir)
    #---------------------------------------------------------------------------------#
    mycol = pft$colour
    mynam = pft$name
    y.txt = desc.unit(desc = description, unit = unit)

    #---------------------------------------------------------------------------------------#
    #     Remove all elements of the DBH/PFT class that do not have a single valid cohort   #
    # at any given time.                                                                    #
    #---------------------------------------------------------------------------------------#
    df = list()
    for(i in seqle(run.type)){
      dft = szpft[[i]][[vnam]][,ndbh+1,]
      empty = (is.na(dft) | dft == 0) & !(col(dft) %in% allpft)
      dft[empty] = NA
      dft = reshape2::melt(dft,varnames = c("Year","mpft"), na.rm = T)
      dft$type = i
      dft$index = interaction(dft$mpft,i)
      df = rbind(df,dft)
    }
    #---------------------------------------------------------------------------------------#

    ggplot(df,aes(x=Year,y=value, colour = factor(mpft))) +
      geom_line(aes(group = index, linetype=LETTERS[df$type]), size = .8,
                show.legend = plt.opt$typeleg) +
      geom_point(aes(group = index)) +
      scale_linetype_manual(name= "Run Type", values=lty.stock(seqle(run.type)),labels = run.type) +
      scale_color_manual(name = "PFT",
                         values = setNames(mycol[unique(df$mpft)], unique(df$mpft)),
                         labels = setNames(mynam[unique(df$mpft)], unique(df$mpft))) +
      ylab(y.txt) +
      theme(legend.position = "bottom",
            axis.title   = element_text(size = 16),
            axis.text    = element_text(face = "bold", size = 12, colour = "black"),
            plot.title   = element_text(lineheight = .8, face="bold"),
            legend.text  = element_text(size = 14),
            legend.title = element_text(size = 14, face="bold"),
            legend.key.size = unit(2, "lines"),
            legend.background = element_rect(color="black", fill=NA, size=.5, linetype = 1),
            panel.border = element_rect(color = "black", fill = NA, size=0.5)) +
      ggtitle(description)

    file.name = paste(vnam,"-total",".pdf",sep="")
    ggsave(file.name, plot = last_plot(), device = "pdf", path = outdir,
           width = plt.opt$width, height = plt.opt$height)

    ggsave(file.name, plot = last_plot(), device = "pdf", path = total.outdir,
           width = plt.opt$width, height = plt.opt$height)

  }
}
#------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------#


# qui inizia il grafico per l'altezza #######################################

#----------------- Load settings for this variable.--------------------------------#
thisvar     = tspftdbh[[ntspftdbh]]
vnam        = thisvar$vnam
description = thisvar$desc
unit        = thisvar$e.unit
plog        = thisvar$plog

#---------------------------------------------------------------------------------#
#     Check if the n directory exists.  If not, create it.                        #
#---------------------------------------------------------------------------------#
outdir = file.path(figdir,vnam)
if (! dir.exists(outdir)) dir.create(outdir)
#---------------------------------------------------------------------------------#

df = patch$maxh
for (v in names(df)){

  #---------------------------------------------------------------------------------------#
  #     Remove all elements of the DBH/PFT class that do not have a single valid cohort   #
  # at any given time.                                                                    #
  #---------------------------------------------------------------------------------------#
  empty = (is.na(df[[v]]) | df[[v]] == 0) & !(col(df[[v]]) %in% allpft)
  df[[v]][empty] = NA
  #---------------------------------------------------------------------------------------#

  ylimit = range(df[[v]])
  y.txt = desc.unit(desc = description, unit = unit)

  npatches = length(row.names(df[[v]]))

  for (p in sequence(npatches)){

    mydf = as.data.frame(df[[v]][p,])
    pftnow = substr(colnames(mydf),nchar(colnames(mydf)),nchar(colnames(mydf)))
    colnames(mydf) = pftnow
    mydf = t(mydf)
    mydf = cbind (mydf, newColumn = as.integer(pftnow))
    colnames(mydf) = c("height","pft")
    mydf = as.data.frame(mydf)

    condition = 17 %in% mydf$pft & length(mydf$height[!is.na(mydf$height)]) >= 2
    #condition=T
    if(condition){
      maxh  = sort(mydf$height,decreasing = T)[1]
      maxh2 = sort(mydf$height,decreasing = T)[2]
      condition2 = abs(maxh2 - maxh) < (maxh / 2)
      #condition2=T
      if (condition2){
        ggplot(data = mydf, aes(x=sequence(npftuse), y = height)) +
          geom_bar(stat="identity") +
          scale_x_continuous(breaks = sequence(npftuse), labels = pftuse) +
          ggtitle(paste(description, "time = ", v, "patch =", p, sep=" "))

        file.name = paste(vnam,"-patch", p,"-",v, ".pdf",sep="")
        ggsave(file.name, plot = last_plot(), device = "pdf", path = outdir,
               width = plt.opt$width, height = plt.opt$height)
      }
    }
  }
}

# qui finisce il grafico per l'altezza #######################################
