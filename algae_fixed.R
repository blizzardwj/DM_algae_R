###################################################
### Loading the Data into R
###################################################
library(DMwR)
#head(algae)
setwd("~/workspace/r-base")
algae <- read.table('Analysis_Correlation.csv',
                    header=T,
                    dec='.',
                    sep = ",",
                    col.names=c('no.','season','size','speed','mxPH','mnO2','Cl',
                                'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4',
                                'a5','a6','a7'),
                    na.strings=c('XXXXXXX'))


colum <- list('no.','season','size','speed','mxPH','mnO2','Cl',
              'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4',
              'a5','a6','a7')
###################################################
### Data Visualization and Summarization
###################################################
sink("Summary_fixed.txt")   #output into summary.txt
summary(algae)
sink()

library(car)
for (i in 5:19) {
  a <- colum[[i]]
  
  png(file=paste('Histogram of ',a,'fixed.png') )
  #par(mfrow=c(1,2))
  hist(algae[,i], prob=T, xlab='',main=paste('Histogram of ',a))
  lines(density(algae[,i],na.rm=T))
  rug(jitter(algae[,i]))
  dev.off( )
  png(file=paste('QQ of ',a,'fixed.png') )
  qqPlot(algae[,i],main=paste('Normal QQ plot of ',a),ylab= as.character(a))
  dev.off( )
  
  
  png(file=paste('box of ',a,'fixed.png') )
  #par(mfrow=c(1,1))
  boxplot(algae[,i],ylab= as.character(a))
  rug(jitter(algae[,i]),side=2)
  abline(h=mean(algae[,i],na.rm=T),lty=2)
  dev.off( )
  #identify(algae[,i])
}

library(lattice)
for (i in 13:19) {
  a <- colum[[i]]
  png(file=paste('Season and ',a,'fixed.png') )
  print(bwplot(season ~ algae[,i], data=algae,ylab='Season',xlab=paste('Algal ',a)))
  dev.off( )
  png(file=paste('River Size and ',a,'fixed.png') )
  print(bwplot(size ~ algae[,i], data=algae,ylab='River Size',xlab=paste('Algal ',a)))
  dev.off( )
  png(file=paste('River Speed and ',a,'fixed.png') )
  print(bwplot(speed ~ algae[,i], data=algae,ylab='River Speed',xlab=paste('Algal ',a)))
  dev.off( )
}
