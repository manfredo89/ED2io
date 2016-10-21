#copy of Rfile_Manfredo_Undisturbed.R
setwd("~/Desktop/Rprova/inputed")
ls()
remove(list=ls())

#Lianas input file
Undist_liana <- as.data.frame(read.csv("DATASHEETS_undisturbed.csv",sep=";", stringsAsFactors = FALSE))
Undist_liana <- Undist_liana[,c("Liananr", "Treenr", "Nearest.tree", "DBH", "Plotnr","LianaEpiphyte")]
#Trees input file
Undist_trees <- as.data.frame(read.csv("Data_Undisturbed_trees.csv",sep=";", stringsAsFactors = FALSE))
Undist_trees <- Undist_trees[,c("Famille", "Species", "X", "Y", "Plotnr", "DBH2015", "Treenr")]
colnames(Undist_trees)[colnames(Undist_trees)=="DBH2015"] <- "DBH"
#Select correct host tree number
Undist_liana$Treenr <- as.character(Undist_liana$Treenr)
Undist_liana$Nearest.tree <- as.character(Undist_liana$Nearest.tree)
Undist_liana$Treenr <- ifelse(is.na(Undist_liana$Treenr), Undist_liana$Nearest.tree, Undist_liana$Treenr)
#Remove the Epiphytes
Undist_liana$LianaEpiphyte <- ifelse(Undist_liana$LianaEpiphyte %in% "E",NA,Undist_liana$LianaEpiphyte)
Undist_liana <- Undist_liana[!is.na(Undist_liana[,"LianaEpiphyte"]),]
#Associate XY liana values to host tree (brute force algorithm, needs optimization)
for (i in 1:nrow(Undist_liana)){
  for (j in 1:nrow(Undist_trees)){
    if(Undist_liana[i,"Plotnr"] == Undist_trees[j,"Plotnr"]){
      if (!is.na(Undist_liana[i,"Treenr"]) & Undist_liana[i,"Treenr"] == Undist_trees[j,"Treenr"]){
        
        Undist_liana[i,"X"] <- Undist_trees[j,"X"]
        Undist_liana[i,"Y"] <- Undist_trees[j,"Y"]

      }
    }
  }
}

#Correct the tree inventory (needs to be completed)
Undist_trees$Famille <- gsub ("Papilionaceae", "Fabaceae", Undist_trees$Famille)
Undist_trees$Famille <- gsub ("Cecropiaceae", "Urticaceae", Undist_trees$Famille)
#Switch from commas to dots
Undist_trees$DBH <- gsub(",",".",Undist_trees$DBH)
Undist_liana$DBH <- gsub(",",".",Undist_liana$DBH)
Undist_trees$DBH <- gsub(",",".",Undist_trees$DBH)
Undist_trees$X <- gsub(",",".", Undist_trees$X) 
Undist_trees$Y <- gsub(",",".", Undist_trees$Y)
Undist_liana$X <- gsub(",",".", Undist_liana$X)
Undist_liana$Y <- gsub(",",".", Undist_liana$Y)
#Set the missing DBH to NA
Undist_trees$DBH[which(Undist_trees$DBH==0)] = NA
#Remove the trees with no DBH
Undist_trees <- Undist_trees[!is.na(Undist_trees[,"DBH"]),]
#get wood density
Wood_density_db <- as.data.frame(read.csv("GWDD.csv",sep=";"))
Wood_density_db$Wood.density <- gsub(",",".", Wood_density_db$Wood.density) 
Wood_density_db$Wood.density <- as.numeric(Wood_density_db$Wood.density)
Wood_density_db$Species <- as.character(Wood_density_db$Species)
#set wood density
Wood_density_db <- as.data.frame(tapply(Wood_density_db$Wood.density,Wood_density_db$Species,mean))
colnames(Wood_density_db)[1] <- "WD"
Wood_density_db$Species <- row.names(Wood_density_db)
source("Merge_with_order_function.R")
Undist_trees <- merge.with.order( Undist_trees, Wood_density_db, by='Species', all.x = T, sort=F ,keep_order = 1)
Undist_trees$WD <- as.numeric(Undist_trees$WD)

Undist_liana$WD <- 0.6

#Assign WD to trees that do not have it
for (i in 1:nrow(Undist_trees)){
  
while(is.na(Undist_trees[i,"WD"])){
  
  Undist_trees[i,"WD"] <- sample (Undist_trees$WD, size = 1)
}
  
}
#Random coordinates for lianas missing them
for (i in 1:nrow(Undist_liana)){
  
  if (is.na(Undist_liana[i,"X"]) | is.na(Undist_liana[i,"Y"])){
    #if (Undist_liana[i,"Plotnr"] == 6)
     # Undist_liana[i,c("X","Y")] <- round(runif(2) * plot6size, 1)
    #else
     # Undist_liana[i,c("X","Y")] <- round(runif(2) * plotsize, 1)
    Undist_liana[i,c("X","Y")] <- 0.0
  }
}
#Create a tag index (needed for ED)
max_trees_in_plot <- vector(mode = "integer", length = nplots)
max_trees_in_plot <- rep.int(0,nplots)

for (i in 1:nplots )
  for (j in 1:nrow(Undist_trees))
    if (Undist_trees[j,"Plotnr"] == i)
      if (Undist_trees[j, "Treenr"] > max_trees_in_plot[i])
        max_trees_in_plot[i] = Undist_trees[j, "Treenr"]

for (i in 1:nrow(Undist_liana))
  Undist_liana[i, "Liananr"] <- Undist_liana[i, "Liananr"] + max_trees_in_plot[Undist_liana[i, "Plotnr"]]

#Merge lianas and trees vertically
Undist_liana$Species <- "Liana"
Undist_liana$Famille <- "Climber"
colnames(Undist_liana)[colnames(Undist_liana)=="Liananr"] <- "tag"
colnames(Undist_trees)[colnames(Undist_trees)=="Treenr"] <- "tag"
#if (gnuplot_stuff)
#pft.idx     = c(2,3,4)
#pft.mid.rho = c(0.53,0.71,0.90)
#npft        = length(pft.mid.rho)
#pft.brks    = c(-Inf,0.5*(pft.mid.rho[-1]+pft.mid.rho[-npft]),Inf)
#pft.cut     = as.numeric(cut(as.numeric(Undist_trees$WD),pft.brks))
#Undist_trees$PFT  = pft.idx[pft.cut]
#Undist_liana$PFT = 17
#endif gnuplot_stuff
Undist_trees <- Undist_trees[,c("Plotnr", "tag", "Species", "Famille", "X", "Y", "WD", "DBH")]
Undist_liana <- Undist_liana[,c("Plotnr", "tag", "Species", "Famille", "X", "Y", "WD", "DBH")]

total <- rbind (Undist_trees, Undist_liana)

total[total==""] <- NA
total <- na.omit(total)

total$X <- as.numeric(as.character(total$X))
total$Y <- as.numeric(as.character(total$Y))
total$DBH <- as.numeric(as.character(total$DBH))

total <- total[order(total$Plotnr), ]

colnames(total) <- c("plots", "tag", "scientific", "family", "x", "y", "wood.dens", "dbh")

write.csv (total, file = "paracou_manfredo.csv", quote = FALSE, row.names=FALSE)
#gnuplot_stuff
#write.table (total, file = "paracou_manfredo_initial.dat", row.names=FALSE, col.names=FALSE, append=FALSE, sep = " ")
#endif