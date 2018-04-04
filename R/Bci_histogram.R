#Script to convert the BCI inventory to the array used to plot the size distribution
dbh=read.table("/Users/manfredo/Desktop/bci_size.txt")
dbh=as.double(dbh[,1])
dbh = dbh / 10.0
this.classdbh = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
h=hist(dbh, breaks = this.classdbh, right=T,freq = T)
#28.8 because there are 8 plots of 60x60m (28800m^2 = 28.8ha)
h$counts=h$counts/28.8
plot(h)
bci=c(sum(h$counts[1:2]),h$counts[3:16],sum(h$counts[17:20]),sum(h$counts[21:24]))
#dput(round(bci,digits=3))
