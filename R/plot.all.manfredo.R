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
library (R.utils)


#==========================================================================================#
#      Here is the user defined variable section.                                          #
#==========================================================================================#


#------------------------------------------------------------------------------------------#
#                                  Paths                                                   #
#------------------------------------------------------------------------------------------#
# For now a maximum of two entries is supported, please don't enter more
# To increase, modify: yeara you can probably use intersert function
args         <- commandArgs(TRUE)
arg.runtype  <- as.character(args[])
if (length(args)==0) {
  cat("No arguments were passed, defaulting to current \n")
  arg.runtype="current"
}
arg.runtype   = c("current")
here          = "/Users/manfredo/Desktop/r_minimal/ED2io/R/"
#site.dir     = paste(here,"../",sep="/")
site.dir      = "/Users/manfredo/Documents/Eclipse_workspace/ED2/ED/build/post_process/paracou/"
run.type      = arg.runtype
if (length(run.type) > 2 || length(run.type) < 1){
  cat ("Error: comparison mode works with a maximum of two simulations.")
  cat ("Please enter a maximum of two arguments.")
  stop(1)
}
# sort data to have it consistentz
run.type = run.type[order(nchar(run.type), run.type)]
is.comparison = length(run.type) > 1
setwd(here)

#------------------------------------------------------------------------------------------#
#                             Time options                                                 #
#------------------------------------------------------------------------------------------#
reload.data    = T                               # Should I reload partially loaded data?
sasmonth.short = c(2,5,8,11)                        # Months for SAS plots (short runs)
sasmonth.long  = 5                                  # Months for SAS plots (long runs)
nyears.long    = 15                                 # Max number of years for short run

#------------------------------------------------------------------------------------------#
#                             Plot options                                                 #
#------------------------------------------------------------------------------------------#
plt.opt = list()
plt.opt$height  = 6.8          # plot height
plt.opt$width   = 8.8          # plot width
# When the length of the simulations differe, should I plot the common region or the whole
# simultion time (used for comparisons)
only.common.part = T
plot.nplant.hystogram = T

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
if(is.comparison)
  com.list         = com.list[duplicated(com.list)]


#------------------------------------------------------------------------------------------#
#              Check if the figures directory exists.  If not, create it.                  #
#------------------------------------------------------------------------------------------#
# hey here it's maybe better to select a different way to name the folder, like this is
# not safe and perhaps misleading
if (length(file.dir) <= 1){
  figdir = file.path(file.dir, "figures")
} else {
  dir.make(paste(site.dir,"comparison",sep=""))
  dirname = paste0(run.type,collapse="_")
  figdir = file.path(paste(site.dir,"comparison/",dirname,sep=""))
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


yeara = as.integer(yeara)        # convert yeara to factor
yearz = as.integer(yearz)        # convert yearz to factor

#attenzione c'è il formato .bz2 fare una cosa piu generale
if( any(!file.exists(paste(analy.path.place,"-Q-",yeara,"-01-00-000000-g01.h5",sep = ""))))
  yeara = yeara + 1
if( any(!file.exists(paste(analy.path.place,"-Q-",yearz,"-12-00-000000-g01.h5",sep = ""))))
  yearz = yearz - 1
#for trials so to shorten things up
yeara = 1500
yearz = 1600

#---------------------------------------------------------------------------------------#
#                    Check time margin consistency                                      #
#---------------------------------------------------------------------------------------#
  cat(" - Yeara:  ",yeara,"\n")
  cat(" - Yearz:  ",yearz,"\n")
if (yeara >= yearz){
  cat(" - Prefix: ",analy.path,"\n")
  cat(" - Invalid years, will not process data...","\n")
  stop(1)
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

yfac  = list()
patch = list()
szpft = list()
dbhds = list()
runn = 1
for (csite in file.dir){
csite="/Users/manfredo/Documents/Eclipse_workspace/ED2/ED/build/post_process/paracou/current"
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

    #----- Update status file with latest data converted into R. ---------------------------#
    latest = paste(datum$year[ntimes],datum$month[ntimes],sep=" ")
    dummy  = write(x=latest,file=ed22.status[runn],append=FALSE)
    #---------------------------------------------------------------------------------------#

    }#end if (! complete)
  #---------------------------------------------------------------------------------------#

  #----- Make some shorter versions of some variables. -----------------------------------#
  # I don't think it makes sense to specify multiple patch variables (patch[[runn]])      #
  # since we don't want to compare maxh for different runs                                #
  szpft[[runn]] = datum$szpft
  yfac[[runn]]  = datum$year
  #---------------------------------------------------------------------------------------#
  # To order data timewise uncomment next command
  # View(lapply(cohort, "[[", "y2013m02"))
  runn = runn + 1
}
patch = datum$patch
dbhds = datum$dbhds

#=======================================================================================#
#                          Plotting section starts here                                 #
#=======================================================================================#


# All PFTs (including total) I have to choose looking at both simulations
if (is.comparison){
  allpft_1    = which(apply(X=szpft[[1]]$nplant,MARGIN=3,FUN=sum,na.rm=TRUE) > 0.)
  allpft_2    = which(apply(X=szpft[[2]]$nplant,MARGIN=3,FUN=sum,na.rm=TRUE) > 0.)
  allpft      = sort(unique(c(allpft_1,allpft_2)))
} else {
allpft      = which(apply(X=szpft[[1]]$nplant,MARGIN=3,FUN=sum,na.rm=TRUE) > 0.)
}

# Remove total
pftuse      = allpft[allpft != (npft+1)]
# Number of PFTs in use
npftuse     = length(pftuse)
# PFT colours
mycol       = pft$colour
# PFT names
mynam       = pft$name

#---------------------------------------------------------------------------------------#
#     Consolidate the yearly means for the long-term dynamics (the PFT and DBH/PFT      #
# stuff).                                                                               #
#---------------------------------------------------------------------------------------#
cat ("    - Finding the annual statistics for multi-dimensional variables...","\n")
cat ("      * Aggregating the annual mean of PFT-DBH variables...","\n")
for(i in seqle(run.type)){
  for (vname in names(szpft[[i]]))
    szpft[[i]][[vname]] = qapply(X=szpft[[i]][[vname]],INDEX=yfac[[i]],DIM=1,
                                 FUN=mean,na.rm=TRUE)


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
#----------------------- total PFT plots -----------------------------------#
#---------------------------------------------------------------------------#
if(nyears >= 2){
  total.outdir = file.path(figdir,"totals")
  if (! dir.exists(total.outdir)) dir.create(total.outdir)

  for(n in sequence(ntspftdbh)){
    #----- Load settings for this variable.-------------------------------------------#
    thisvar     = tspftdbh[[n]]
    vnam        = thisvar$vnam
    description = thisvar$desc
    unit        = thisvar$e.unit
    plog        = thisvar$plog

    if (! vnam %in% names(szpft[[1]])) next

    mycol = pft$colour
    mynam = pft$name
    y.txt = desc.unit(desc = description, unit = unit)

    #---------------------------------------------------------------------------------------#
    #     Prepare the variable in the ggplot friendly format:                               #
    #                                                                                       #
    #     -Remove all elements of the DBH/PFT class that do not have a single valid cohort  #
    #     at any given time.                                                                #
    #                                                                                       #
    #     -Melt data so that we have all PFTs is one column                                 #
    #                                                                                       #
    #     -Add the corresponding indexes PFT, type which spans over the tuns we want to     #
    #     compare and index which is a unique combination of the previous 2 (just to order) #
    #                                                                                       #
    #---------------------------------------------------------------------------------------#
    df = list()
    for(i in seqle(run.type)){
      if(only.common.part){
        dft = szpft[[i]][[vnam]][1:nyears,ndbh+1,]
      } else {
        dft = szpft[[i]][[vnam]][,ndbh+1,]
      }

      empty = (is.na(dft) || dft == 0.0)
      dft = dft[, allpft]
      colnames(dft) = allpft
      dft = reshape2::melt(dft,varnames = c("Year","mpft"))
      dft$type = i
      dft$index = interaction(dft$mpft,i)
      df = rbind(df,dft)
    }
    # Set the simulation to terminate at year 2000
    df$Year = df$Year + 2000 - (nyears + yeara)
    #---------------------------------------------------------------------------------------#

    p = ggplot(df,aes(x=Year,y=value, colour = factor(mpft))) +
      geom_line(aes(group = index, linetype=LETTERS[df$type]), size = .8) +
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
      ggtitle(description) +
      theme(plot.title = element_text(hjust = 0.5))

    if (! is.comparison) p = p + guides(linetype = FALSE)
    file.name = paste(vnam,"-total",".pdf",sep="")

    ggsave(file.name, plot = p, device = "pdf", path = total.outdir,
           width = plt.opt$width, height = plt.opt$height)

  }
} else cat("Less than 2 years of output, aborting Total plots!")
#------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------#


if(plot.nplant.hystogram){

  exp   = list()
  #---------------------------------------------------------------------------------#
  exp[["liana_clss"]] = cbind(exp[["liana_clss"]],dist=c(0,117.1,53.594,28.79,15.454,12.631,9.331,5.552,
                                         5.357,3.728,2.816,1.188,0.754,0.573,0.5,0.5,0.5))
  exp[["liana_clss"]] = cbind(exp[["liana_clss"]],undist=c(0,52,31.831,20.801,10.735,10.29,5.996,4.11,
                                           2.701,2.258,1.814,1.369,0.5,0.5,0.5,0.5,0.5))

  exp[["tree_clss"]] = cbind(exp[["tree_clss"]],dist=c(0,262.07,140.36,93.11,57.27,39.89,33.92,
                                        30.12,12.74,4.59,3.51,1.88,0.5,0.5))
  exp[["tree_clss"]] = cbind(exp[["tree_clss"]],undist=c(0,203.36,102.89,63.24,40.98,25.77,
                                          22.51,33.92,16,8.39,3.51,2.96,0.5,0.5))
  
  bci = c(0, 78.889, 42.604, 23.09, 10.764, 6.736, 3.368, 1.667, 
          1.007, 0.799, 0.729, 0.312, 0.347, 0.104, 0.069, 0.069, 0.208)

  #------ Set up the title and axis labels. ----------------------------------#
  lexlab="DBH Classes [cm]"
  leylab  = desc.unit(desc="Plant density",unit="p*l*a*n*t^phantom(1)*h*a^{-1}")
  #---------------------------------------------------------------------------#

  # Set colors and names
  cols=c(mycol[pftuse],"#363939","#838B8B")
  nams=c(mynam[pftuse],"Disturbed plot", "Undisturbed plot")


  histo.outdir = file.path(figdir,"size_class")
  if (! dir.exists(histo.outdir)) dir.create(histo.outdir)

  cat("    + Nplant histograms...","\n")
  for (i in names(dbhds)){

    temp = qapply(X=dbhds[[i]], INDEX=yfac[[1]], DIM=1, FUN=mean, na.rm=TRUE)

    if (i == "tree_clss"){
      this.classdbh   = c(0,10,15,20,25,30,35,40,50,60,70,80,90,100)
    } else {
      this.classdbh   = c(0,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,20)
    }

    this.ndbh     = length(this.classdbh)
    this.breakdbh = c(-Inf,this.classdbh[-1],Inf)
    this.dbhnames     = paste( c("<",paste("[",this.classdbh[-c(1,this.ndbh)],"-",sep=""),">")
                              , c(this.classdbh[2],paste(this.classdbh[-c(1,2)],"]",sep=""),
                                  this.classdbh[this.ndbh])
                              , sep="")

    if(length(this.dbhnames) > 10){
      for(j in 3:(length(this.dbhnames) - 1)){
        if(j%%2 == 1 | this.dbhnames[j] == "[90-100]") this.dbhnames[j] = " "
      }
    }


    #---------------------------------------------------------------------------------#
    #     Retrieve the variable, and keep only the part that is usable.               #
    #---------------------------------------------------------------------------------#
    nyaverage = min(nyears,50)
    thisvnam  = temp[,-(this.ndbh+1),pftuse]
    ystart    = max(1, nyears-nyaverage)
    yend      = nyears
    # 10000 is the multiplication by ha conversion
    # my.avg is the averaged nplants
    my.avg    = apply(thisvnam[ystart:yend,,], c(2, 3), mean, na.rm = TRUE) * 10000

    # Melt the data in the proper format
    max.dbh=as.character(this.classdbh)
    ggdf=as.data.frame(my.avg)

    #Add the field data (undisturbed then disturbed)
    if(arg.runtype=="bci" | arg.runtype=="bci0"){
      if(i=="liana_clss"){
        ggdf=cbind(ggdf, bci)
        colnames(ggdf)=c(pftuse,5)
      } else { colnames(ggdf)=pftuse }
      
    } else {
      
    ggdf=cbind(ggdf, exp[[i]][,2], exp[[i]][,1])
    colnames(ggdf)=c(pftuse,5,6)
    
    }

    # When using lianas PFT 5 and 6 should not be used... (dirty hack)
    ggdf=cbind(ggdf,max.dbh)

    ggdf=reshape2::melt(ggdf,id.vars = "max.dbh",variable.name="pft",value.name="nplant")

    # Set the first bin to 0 as it is always too large (saplings)
    ggdf[ggdf$max.dbh==0,]$nplant = 0

    # This is to have the correct order of levels
    ggdf$max.dbh = factor(ggdf$max.dbh, levels = this.classdbh)

    # Start the plot
    ggp = ggplot(ggdf,aes(x=max.dbh,y=nplant,fill=pft))

    if(arg.runtype=="bci" | arg.runtype=="bci0"){
      if (i == "tree_clss"){
        ggp = ggp +
          geom_bar(data=subset(ggdf,pft%in%c(2,3,4)),aes(x=as.numeric(max.dbh)-0.2),stat="identity", position="stack", width=0.4) +
          geom_bar(data=subset(ggdf,pft==17),aes(x=as.numeric(max.dbh)+0.2),stat="identity", position="dodge", width=0.4) +
          scale_fill_manual(name = "KIND", values = cols[c(4,1,2,3)], labels=nams[c(4,1,2,3)])
        
      } else {
        ggp = ggp +
          geom_bar(data=subset(ggdf,pft%in%c(5,17)),stat="identity", position="dodge",width=0.75) +
          scale_fill_manual(name = "KIND", values = cols[c(4,5)], labels=c(nams[4],"50ha plot"))
      }
      
    } else {
      
      if (i == "tree_clss"){
        ggp = ggp +
          geom_bar(data=subset(ggdf,pft%in%c(2,3,4)),aes(x=as.numeric(max.dbh)-0.22),stat="identity", position="stack", width=0.22) +
          geom_bar(data=subset(ggdf,pft%in%c(5,6)),aes(x=as.numeric(max.dbh)+0.11),stat="identity", position="dodge", width=0.44) +
          scale_fill_manual(name = "KIND", values = cols[c(1,2,3,6,5)], labels=nams[c(1,2,3,6,5)])
      } else {
        ggp = ggp +
          geom_bar(data=subset(ggdf,pft%in%c(5,6,17)),stat="identity", position="dodge",width=0.66) +
          scale_fill_manual(name = "KIND", values = cols[c(4,6,5)], labels=nams[c(4,6,5)])
      }
    }
    ggp = ggp +
      scale_x_discrete(limits=max.dbh,labels = this.dbhnames) +
      xlab(lexlab) +
      ylab(leylab) +
      guides(fill=guide_legend(ncol=2)) +
      theme(legend.position=c(0.6,0.8),
            axis.title   = element_text(size = 16),
            axis.text    = element_text(face = "bold", size = 12, colour = "black"),
            plot.title   = element_text(lineheight = .8, face="bold"),
            legend.text  = element_text(size = 14),
            legend.title = element_text(size = 14, face="bold"),
            legend.key.size = unit(1.5, "lines"),
            legend.background = element_rect(color="black", fill="gray90", size=.5, linetype = 1),
            panel.border = element_rect(color = "black", fill = NA, size=0.5)) +
      ggtitle("Size distribution") +
      theme(plot.title = element_text(hjust = 0.5))

    file.name = paste(i,".pdf",sep="")

    ggsave(file.name, plot = ggp, device = "pdf", path = histo.outdir,
           width = plt.opt$width, height = plt.opt$height)

  }
}

#---------------------------------------------------------------------------------#
#     Check if the n directory exists.  If not, create it.                        #
#---------------------------------------------------------------------------------#
patch_plot_dir = file.path(figdir,"patch_plots")
if (! dir.exists(patch_plot_dir)) dir.create(patch_plot_dir)
#---------------------------------------------------------------------------------#

#-------- Here we will do the height graph to track lianas and trees heights ------#
npatches = 24
for (n in sequence(npatch_plots)){
  #----------------- Load settings for this variable.--------------------------------#
  thisvar     = patch_plots[[n]]
  vnam        = thisvar$vnam
  description = thisvar$desc
  unit        = thisvar$e.unit
  plog        = thisvar$plog
  
  #---------------------------------------------------------------------------------#
  #     Check if the n directory exists.  If not, create it.                        #
  #---------------------------------------------------------------------------------#
  outdir = file.path(patch_plot_dir,vnam)
  if (! dir.exists(outdir)) dir.create(outdir)
  #---------------------------------------------------------------------------------#
  
  df = patch[[vnam]]
  y.txt = desc.unit(desc = description, unit = unit)
  
  #We need to fill the missing plants heights with NAs
  for(name in names(df)){
    
    colnames = paste("X",pftuse,sep="")
    
    for (t in seqle(pftuse)){
      
      column = colnames[t]
      if (! column %in% colnames(df[[name]])){
        
        df[[name]][[column]] = rep(NA,max(as.numeric(lapply(df[[name]], function(x) length(x)))))
      }
    }
    df[[name]] = df[[name]][,colnames]
  }
  
  for (p in sequence(npatches)){
    mydf = NULL
    for(t in seqle(pftuse)){
      tempdf = as.numeric(lapply(df, function(x) unlist(x[t])[p]))
      # Add the coordinates of the 12 months
      tempdf = cbind (yfac[[1]] + rep(seq(0,0.99,0.083333333),nyears), tempdf, pftuse[t])
      mydf = rbind(mydf, tempdf)
    }
    mydf = as.data.frame(mydf)
    colnames(mydf) = c("Year","height","pft")
    
    ggp = ggplot(mydf,aes(x=Year, y=height, group = pft, colour = factor(pft))) +
      geom_point(data=subset(mydf, pft != 17)) +
      geom_line() +
      scale_color_manual(name = "PFT", values = setNames(mycol[mydf$pft],mydf$pft),
                         labels = setNames(mynam[mydf$pft],mydf$pft)) +
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
    theme(plot.title = element_text(hjust = 0.5))
    
    file.name = paste("patch-",p,".pdf",sep="")
    
    ggsave(file.name, plot = ggp, device = "pdf", path = outdir,
           width = plt.opt$width, height = plt.opt$height)
  }
}
