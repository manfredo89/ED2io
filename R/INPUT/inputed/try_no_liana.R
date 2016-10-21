setwd("~/Desktop/Rprova/inputed")
#copy of Rfile_Manfredo_Undisturbed.R
ls()
remove(list=ls())

#Useful variables
plotsize <- 70
plot6size <- 100
nplots <- 7

#Trees input file
Undist_trees <- as.data.frame(read.csv("Data_Undisturbed_trees.csv",sep=";", stringsAsFactors = FALSE))
Undist_trees <- Undist_trees[,c("Famille", "Species", "X", "Y", "Plotnr", "DBH2015", "Treenr")]
colnames(Undist_trees)[colnames(Undist_trees)=="DBH2015"] <- "DBH"

#Correct the tree inventory (needs to be completed)
Undist_trees$Famille <- gsub ("Papilionaceae", "Fabaceae", Undist_trees$Famille)
Undist_trees$Famille <- gsub ("Cecropiaceae", "Urticaceae", Undist_trees$Famille)
#Switch from commas to dots
Undist_trees$DBH <- gsub(",",".",Undist_trees$DBH)
Undist_trees$DBH <- gsub(",",".",Undist_trees$DBH)
Undist_trees$X <- gsub(",",".", Undist_trees$X) 
Undist_trees$Y <- gsub(",",".", Undist_trees$Y)
#Set the missing DBH to NA
Undist_trees$DBH[which(Undist_trees$DBH==0)] = NA
#Remove the trees with no DBH
Undist_trees <- Undist_trees[!is.na(Undist_trees[,"DBH"]),]
#plot
#plot(hist(as.numeric(Undist_trees$DBH2015), xlim=c(2,140),las=1,breaks=350))
#bio trees
Wood_density_db <- as.data.frame(read.csv("GWDD.csv",sep=";"))
Wood_density_db$Wood.density <- gsub(",",".", Wood_density_db$Wood.density) 
Wood_density_db$Wood.density <- as.numeric(Wood_density_db$Wood.density)
Wood_density_db$Species <- as.character(Wood_density_db$Species)

Wood_density_db <- as.data.frame(tapply(Wood_density_db$Wood.density,Wood_density_db$Species,mean))
colnames(Wood_density_db)[1] <- "WD"
Wood_density_db$Species <- row.names(Wood_density_db)
source("Merge_with_order_function.R")
Undist_trees <- merge.with.order( Undist_trees, Wood_density_db, by='Species', all.x = T, sort=F ,keep_order = 1)
Undist_trees$WD <- as.numeric(Undist_trees$WD)


#Assign WD to trees that do not have it
for (i in 1:nrow(Undist_trees)){
  
  while(is.na(Undist_trees[i,"WD"])){
    
    Undist_trees[i,"WD"] <- sample (Undist_trees$WD, size = 1)
  }
  
}
#Create a tag index (needed for ED)
max_trees_in_plot <- vector(mode = "integer", length = nplots)
max_trees_in_plot <- rep.int(0,nplots)

colnames(Undist_trees)[colnames(Undist_trees)=="Treenr"] <- "tag"

Undist_trees <- Undist_trees[,c("Plotnr", "tag", "Species", "Famille", "X", "Y", "WD", "DBH")]

total <- Undist_trees

total[total==""] <- NA
total <- na.omit(total)

total$X <- as.numeric(as.character(total$X))
total$Y <- as.numeric(as.character(total$Y))
total$DBH <- as.numeric(as.character(total$DBH))
total$WD <- as.numeric(as.character(total$WD))


write.csv (total, file = "paracou_manfredo_no_lianas.csv", quote = FALSE, row.names=FALSE)