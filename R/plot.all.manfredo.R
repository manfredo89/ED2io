#==========================================================================================#
#' @title Main plot utility
#' @author Marcos Longo & Manfredo di Porcia
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
exp.dir       = "/Users/manfredo/Desktop/size_distr/area/"
run.type      = arg.runtype
if (length(run.type) > 2 || length(run.type) < 1){
  cat ("Error: comparison mode works with a maximum of two simulations.")
  cat ("Please enter a maximum of two arguments.")
  stop(1)
}
# sort data to have it consistent
run.type = sort(run.type)
is.comparison = length(run.type) > 1
setwd(here)

#------------------------------------------------------------------------------------------#
#                             Time options                                                 #
#------------------------------------------------------------------------------------------#
reload.data    = F                               # Should I reload partially loaded data?
sasmonth.short = c(2,5,8,11)                        # Months for SAS plots (short runs)
sasmonth.long  = 5                                  # Months for SAS plots (long runs)
nyears.long    = 15                                 # Max number of years for short run
sampling       = c(20,30)                          # Years where I need to average / sample

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
for (i in seqle(run.type)) file.dir[i] = paste(site.dir, run.type[i], sep = "")


analy.path       = paste(file.dir,"analy",sep='/')  # analysis folder
analy.path.place = paste(analy.path,place,sep='/')   # same with locus prefix
com.list         = list.files(analy.path, pattern = "*-Q-*")
if(is.comparison) com.list = com.list[duplicated(com.list)]


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
#yeara = 1508
#yearz = 1930

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
emean = list()
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
  } else {
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
    latest = paste(datum$year[m],datum$month[m],sep=" ")
    dummy  = write(x=latest,file=ed22.status[runn],append=FALSE)
    #---------------------------------------------------------------------------------------#

  }#end if (! complete)
  #---------------------------------------------------------------------------------------#

  #----- Make some shorter versions of some variables. -----------------------------------#
  # I don't think it makes sense to specify multiple patch variables (patch[[runn]])      #
  # since we don't want to compare maxh for different runs                                #
  szpft[[runn]] = datum$szpft
  yfac [[runn]] = datum$year
  dbhds[[runn]] = datum$dbhds
  patch[[runn]] = datum$patch
  emean[[runn]] = datum$emean
  #---------------------------------------------------------------------------------------#
  # To order data timewise uncomment next command
  # View(lapply(cohort, "[[", "y2013m02"))
  runn = runn + 1
}

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
#              Set the ggplot theme to recycle in all the graphs                        #
#---------------------------------------------------------------------------------------#

th = theme(legend.position = "bottom",
           legend.box   = "vertical",
           axis.title   = element_text(size = 22),
           axis.text    = element_text(size = 20, colour = "black"),
           plot.title   = element_text(size = 24,lineheight = .8, face="bold", hjust = 0.5),
           legend.text  = element_text(size = 18),
           legend.title = element_text(size = 14, face="bold"),
           legend.key.size = unit(0.8, "cm"),
           legend.background = element_rect(color="black", fill=NA, size=.5, linetype = 1),
           panel.background = element_blank(),
           panel.border = element_rect(color = "black", fill = NA, size=.5),
           panel.grid.major = element_line(colour = "grey75"))
#---------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------#
#     Consolidate the yearly means for the long-term dynamics (the PFT and DBH/PFT      #
# stuff).                                                                               #
#---------------------------------------------------------------------------------------#
cat ("    - Finding the annual statistics for multi-dimensional variables...","\n")
cat ("      * Aggregating the annual mean of PFT-DBH variables...","\n")
for(i in seqle(run.type)){
  for (vname in names(szpft[[i]])){
    if (vname != "ddbh_dt"){
      szpft[[i]][[vname]] = qapply(X=szpft[[i]][[vname]],INDEX=yfac[[i]],DIM=1,
                                   FUN=mean,na.rm=TRUE)
    } else {
      szpft[[i]][[vname]] = qapply(X=szpft[[i]][[vname]],INDEX=yfac[[i]],DIM=1,
                                   FUN=sum,na.rm=TRUE)
    }
  }
  #---------------------------------------------------------------------------------------#
  #     Remove all elements of the DBH/PFT class that do not have a single valid cohort   #
  # at any given time.                                                                    #
  #---------------------------------------------------------------------------------------#
  empty = is.na(szpft[[i]]$nplant) | szpft[[i]]$nplant == 0
  for (vname in names(szpft[[i]])) szpft[[i]][[vname]][empty] = NA
}
#---------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------#
#----------------------- Time series plots ---------------------------------#
#---------------------------------------------------------------------------#
if(nyears >= 2){

  cat("+ Time series graphs","\n")

  total.outdir = file.path(figdir,"totals")
  if (! dir.exists(total.outdir)) dir.create(total.outdir)

  # Set x axis title outside loop (same for all)
  x.txt = "Time [y]"

  for(n in sequence(ntspftdbh)){
    #----- Load settings for this variable.-------------------------------------------#
    thisvar     = tspftdbh[[n]]
    vnam        = thisvar$vnam
    description = thisvar$desc
    unit        = thisvar$e.unit

    if (! vnam %in% names(szpft[[1]])) next

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
    df  = list()
    for(i in seqle(run.type)){
      if(only.common.part){
        dft = szpft[[i]][[vnam]][1:nyears,ndbh+1,]
      } else {
        dft = szpft[[i]][[vnam]][,ndbh+1,]
      }

      dft = dft[, allpft]
      colnames(dft) = allpft
      dft = reshape2::melt(dft,varnames = c("time","pft"))
      # Set the right line type
      if (i == 1){
        dft$type = as.factor(ifelse(17 %in% dft$pft | length(run.type) == 1 ,"solid","22"))
      } else {
        dft$type = as.factor(ifelse(any(dft$type == "22"),"solid","22"))
      }
      dft$index = interaction(dft$pft,i)
      df = rbind(df,dft)
    }

    # Set the simulation to terminate at year 2000
    #---------------------------------------------------------------------------------------#

    p = ggplot(df,aes(x=time - yeara,y=value, colour = factor(pft), linetype=type)) +
      annotate("rect", fill = "grey", alpha = 0.5, xmin = sampling[1], xmax = sampling[2],
               ymin = -Inf, ymax = Inf) +
      geom_line(aes(group = index), size = 1.4) +
      #scale_linetype_manual(name= "Run Type", values = unique(as.character(df$type)), labels = run.type) +
      scale_linetype_identity("Run Type", labels = run.type, guide="legend") +
      scale_color_manual(name = "PFT",
                         values = setNames(mycol[unique(df$pft)], unique(df$pft)),
                         labels = setNames(mynam[unique(df$pft)], unique(df$pft))) +
      ylab(y.txt) +
      xlab(x.txt) +
      th +
      ggtitle(description)

   # if (! is.comparison){

      mean_nyears = sampling[2] - sampling[1]

      # Compute the averages
      dfm = list()
      dfm = df[df$time %in% tail(df$time, n=mean_nyears),]
      mean_dfm = signif(qapply(X=dfm$value, INDEX=dfm$index, DIM=1, FUN=mean, na.rm=TRUE),4)

      p = p +
        guides(linetype = FALSE) +
        annotate("text", x = 1.005 * nyears, y=df[df$time==yearz,]$value, label = as.character(mean_dfm),color=rep(mycol[allpft],length(run.type)))
    #}

    file.name = paste(vnam,".pdf",sep="")

    ggsave(file.name, plot = p, device = "pdf", path = total.outdir,
           width = plt.opt$width, height = plt.opt$height)

  }
} else cat("Less than 2 years of output, aborting Total plots!","\n")
#------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------#
#------------------------ Size class plots ---------------------------------#
#---------------------------------------------------------------------------#
if(plot.nplant.hystogram){

  cat("+ Size class plots","\n")

  # Set colors and names
  cols=c(mycol[pftuse],"#363939","#838B8B")
  nams=c(mynam[pftuse],"Undisturbed plot", "Disturbed plot")


  histo.outdir = file.path(figdir,"size_class")
  if (! dir.exists(histo.outdir)) dir.create(histo.outdir)

  #------ Set up the title and axis labels. ----------------------------------#
  x.txt ="DBH Classes [cm]"
  y.txt = desc.unit(desc="Basal area",unit=untab$cm2om2)
  #---------------------------------------------------------------------------#

  for (i in names(dbhds[[1]])){

    if (i == "tree_clss"){
      this.classdbh   = c(0,10,15,20,25,30,35,40,50,60,70,80,90,100)
    } else {
      this.classdbh   = c(0,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,20)
    }
    max.dbh=as.character(this.classdbh)
    this.ndbh     = length(this.classdbh)
    this.dbhnames     = paste( c(paste("[",this.classdbh[-c(1,this.ndbh)],"-",sep=""),">")
                               , c(paste(this.classdbh[-c(1,2)],")",sep=""),
                                   this.classdbh[this.ndbh])
                               , sep="")

    if(length(this.dbhnames) > 10){
      for(j in 2:(length(this.dbhnames) - 1)){
        if(j%%2 == 0 | this.dbhnames[j] == "[90-100]") this.dbhnames[j] = " "
      }
    }


    #---------------------------------------------------------------------------------#
    #     Retrieve the variable, and keep only the part that is usable.               #
    #---------------------------------------------------------------------------------#
    nyaverage = sampling[2] - sampling[1]
    mstart    = 1 + 12 * sampling[1]
    mend      = 12 * sampling[2]

    # Set the number of bars
    # model data
    n_mod = length(run.type)
    # experimental data
    n_exp = 1
    nbars = n_mod + n_exp
    mywidth = 0.6 / nbars

    p = ggplot()

    for (k in 1:n_mod){

      # Add transparency for run comparison
      if (k == 1){
        myalpha = 1.0
      } else {
        myalpha = 0.4
      }

      # Add x axis shift depending on how many bars you want
      mydelta = mywidth * ( k - 0.5  + n_exp - nbars / 2)

      # Keep only: the months I want, the dbhs I want and the PFTs I want and then average
      # add * 10000 for the ha conversion
      dft = apply(dbhds[[k]][[i]][mstart:mend,-c(1,this.ndbh+1),pftuse], c(2, 3), mean, na.rm = TRUE)
      # Melt the data
      dft = reshape2::melt(dft, varnames=c("dbh", "pft"), value.name="nplant")
      # Add a dodge coordinate
      dft$dbh = dft$dbh + mydelta

      if(i == "tree_clss"){
        dft = dft[dft$pft!=4,]
      } else {
        dft = dft[dft$pft==4,]
      }

      p = p +
        geom_bar(data=dft,aes(x=dbh,y=nplant, fill=factor(pft)),stat="identity",
                 position="stack", width=mywidth, alpha = myalpha, color= "black")

    }

    #---------------------------------------------------------------------------------#
    # Save in memory the experimental data if needed.
    #---------------------------------------------------------------------------------#
    plot_exp_data=T
    if (plot_exp_data & n_exp > 0){

      dfe = NULL
      #place = "gigante"
      # Now add the experimental data if required (up to two bars)
      for (k in 1:n_exp){

        if(i == "tree_clss"){
          p_dist = read.table(paste(exp.dir,place,"_T_",k,".txt", sep = ""), col.names = "nplant")
        } else {
          p_dist = read.table(paste(exp.dir,place,"_L_",k,".txt", sep = ""), col.names = "nplant")
        }

        p_dist = as.data.frame(p_dist / 10000)
        # Add the experimental data as a new pft
        p_dist = cbind(p_dist, pft = 4+k)
        mydelta = mywidth * ( k - 0.5 - nbars / 2)
        p_dist = cbind(p_dist, dbh = as.numeric(row.names(p_dist)) + mydelta)
        dfe = rbind (dfe, p_dist)

      }

      p = p + geom_bar(data=dfe,aes(x = dbh, y=nplant, fill = factor(pft)),
                       stat="identity", width=mywidth, color = "black")
    }

    p = p + scale_fill_manual(name = "",values = c(unique(cols[dft$pft]),unique(cols[dfe$pft])),
                              labels = c("Model", "Data")) +
      #      p = p + scale_fill_manual(name = "PFT", values = c(unique(cols[dft$pft]),unique(cols[dfe$pft])),
      #      labels = c(unique(nams[dft$pft]), unique(nams[dfe$pft]))) +
      scale_x_discrete(limits=max.dbh[-1],labels = this.dbhnames) +
      xlab(x.txt) +
      ylab(y.txt) +
      guides(fill=guide_legend(ncol=2)) +
      theme(legend.position=c(0.6,0.8),
            axis.title   = element_text(size = 16),
            axis.text    = element_text(face = "bold", size = 12, colour = "black"),
            plot.title   = element_text(lineheight = .8, face="bold",hjust = 0.5),
            legend.text  = element_text(size = 14),
            legend.title = element_blank(),
            legend.key.size = unit(0.8, "cm"),
            legend.background = element_rect(color="black", fill="gray90", size=.5, linetype = 1),
            panel.border = element_rect(color = "black", fill = NA, size=0.5))
    #ggtitle("Size distribution") +

    file.name = paste(i,".pdf",sep="")

    ggsave(file.name, plot = p, device = "pdf", path = histo.outdir,
           width = plt.opt$width, height = plt.opt$height)

  }
}
#------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------#
#----------------------------- Patch plots ---------------------------------#
#---------------------------------------------------------------------------#


cat("+ Age patch plots","\n")

#---------------------------------------------------------------------------------#
#     Check if the n directory exists.  If not, create it.                        #
#---------------------------------------------------------------------------------#
outdir = file.path(figdir,"patch_plots")
if (! dir.exists(outdir)) dir.create(outdir)
#---------------------------------------------------------------------------------#

for (n in sequence(npatch_plots)){
  #----------------- Load settings for this variable.--------------------------------#
  thisvar     = patch_plots[[n]]
  vnam        = thisvar$vnam
  description = thisvar$desc
  unit        = thisvar$e.unit

  gdf = NULL

  for(i in seqle(run.type)){
    df     = NULL
    mydf   = NULL
    tempdf = NULL
    df = patch[[i]][[vnam]]
    df = df[seq(sampling[1],sampling[2])]

    if(is.comparison){
      if(i == 1){
        pft.here = allpft_1
      } else { pft.here = allpft_2}
    } else {
      pft.here = allpft
    }

    for(name in names(df)){
      tempdf = df[[name]]
      colnames(tempdf) = pft.here
      tempdf = cbind (age=patch[[i]]$age[[name]],tempdf)
      mydf = rbind(mydf, tempdf)
    }
    mydf = reshape2::melt(mydf, id.vars="age")
    colnames(mydf)[2] = "pft"

    mydf[is.na(mydf)] = 0.0
    mydf$age = ceiling(mydf$age)
    mydf = aggregate(value~age+pft,data=mydf,mean)

    if (i == 1){
      mydf$type = as.factor(ifelse(17 %in% mydf$pft,"solid","22"))
    } else {
      mydf$type = as.factor(ifelse(any(mydf$type == "22"),"solid","22"))
    }
    mydf$index = interaction(mydf$pft,i)
    gdf = rbind(gdf, mydf)
  }

  gdf = as.data.frame(gdf)
  gdf[is.na(gdf)] = 0.0
  gdf$age = ceiling(gdf$age)
  gdf = aggregate(value~age+pft+type+index,data=gdf,mean)
  gdf$pft = as.numeric(as.character(gdf$pft))

  y.txt = desc.unit(desc = description, unit = unit)

  p = ggplot(gdf,aes(x=age, y=value, colour = factor(pft), linetype=type)) +
    geom_line(aes(group = index),size=1.4) +
    scale_x_log10() +
    scale_y_log10(breaks=c(.01,.1,1,10),labels=c(.01,.1,1,10)) +
    scale_linetype_identity("Run Type", labels = run.type, guide="legend") +
    scale_color_manual(name = "PFT", values = setNames(mycol[unique(gdf$pft)],unique(gdf$pft)),
                       labels = setNames(mynam[unique(gdf$pft)],unique(gdf$pft))) +
    ylab(y.txt) +
    xlab("Gap age [y]") +
    th +
    #ggtitle(description) +
    theme(plot.title = element_text(hjust = 0.5))

  file.name = paste(vnam,".pdf",sep="")

  ggsave(file.name, plot = p, device = "pdf", path = outdir,
         width = plt.opt$width, height = plt.opt$height)
}

#------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------#


#---------------- Plot simple variables ---------------------------------------------#


cat("+ Emean graphs","\n")

emean.outdir = file.path(figdir,"emean")
if (! dir.exists(emean.outdir)) dir.create(emean.outdir)

# Set x axis title outside loop (same for all)
x.txt = "Time [y]"

for(n in sequence(nemean_plots)){
  #----- Load settings for this variable.-------------------------------------------#
  thisvar     = emean_plots[[n]]
  vnam        = thisvar$vnam
  description = thisvar$desc
  unit        = thisvar$unit

  df  = list()
  for(i in seqle(run.type)){

    dft = emean[[i]][[vnam]]
    dft = .colMeans(dft,12,nyears)
    dft = as.data.frame(dft)
    if (i == 1){
      dft$type = as.factor("22")
    } else {
      dft$type = as.factor("solid")
    }
    dft$time = 0:(nyears -1)
    df=rbind(df,dft)
  }

  y.txt = desc.unit(desc = description, unit = unit)

  p = ggplot(df, aes(x=time, y=dft, linetype=type)) +
    geom_line(aes(linetype = type), size = 1.4) +
    scale_linetype_manual("Run Type", values = unique(as.character(df$type)), labels = run.type) +
    ylab(y.txt) +
    xlab(x.txt) +
    th +
    ggtitle(description)

  mean_nyears = sampling[2] - sampling[1]
  # Compute the averages
  dfm = list()
  dfm = df[sampling[1] < df$time & df$time < sampling[2],]
  mean_dfm = signif(qapply(X=df$dft, INDEX=df$type, DIM=1, FUN=mean, na.rm=TRUE),4)

  p = p +
    #guides(linetype = FALSE) +
    annotate("text", x = nyears, y=0.8*df[df$time==nyears -1,]$dft, label = as.character(mean_dfm))

  file.name = paste(vnam,".pdf",sep="")

  ggsave(file.name, plot = p, device = "pdf", path = emean.outdir,
         width = plt.opt$width, height = plt.opt$height)
}


