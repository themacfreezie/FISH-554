############################################################################
#Fit of the model sbtmodxx.tpl to the data for southern bluefin tuna.
#Fits of the observed Indonesian age data to the predicted Indonesian age data.
#Programmed by Trevor A. Branch 29 June 2009
#outputs from a _lab.rep file, assuming that the naming convention in the file has sections of 
#outputs separated by $name comments so that it can be easily parsed by readList() function of Jon Schnute 
#require(PBSmodelling)
#require(gplots)
#Run by:
###source("IndoFitAll.r")
##########################################################################
IndoAgeFits <- function(data.object, case_label="c1s1l1orig.5_h1m1M1O1C2a1") {
   subtle.color <- "gray40"
   x <- data.object
   ages <- x$ages.1   #age range
   obs.data <- x$age.obs.1[,-c(1,2)]
   pred.data <- x$age.pred_1[,-c(1,2)]
   years <- x$age.obs.1[,2]
   
   nyears <- length(years)
   ages.list <- ages[1]:ages[2]
   nages <- length(ages.list)
   mfrow <- c(nyears,1)
   par(mfrow=mfrow,oma=c(4,5.5,3.5,1),mar=c(0,0,0,0))
   
   cohort.color <- rich.colors(nages+nyears)
   
   ylim <- c(0,1.05*max(obs.data,pred.data))
   for (yr in 1:nyears) { 
      names.arg <- rep("",nages)
      
      x <- barplot(obs.data[yr,],space=0.2,ylim=ylim,las=1,names.arg=names.arg, cex.names=0.5, xaxs="i",yaxs="i",col=cohort.color[(nyears+1):(nages+nyears)],
                        axes=F,ylab="",xlab="", border=subtle.color,lwd=0.5)
      cohort.color <- c(cohort.color[length(cohort.color)],cohort.color[-1*length(cohort.color)])  #loop around colors
      if (yr >= nyears) {
         axis(side=1,at=x,lab=ages.list, col.axis=subtle.color, col=subtle.color, lwd=0,lwd.ticks=0)  #just use for the labels, to allow more control than names.arg
      }
      axis(2,las=1,at=c(0,0.1), col=subtle.color, col.axis=subtle.color,lwd=0.5)
      par(new=T)
      par(xpd=NA)
      plot(x=x,y=pred.data[yr,],ylim=ylim, xlim=par("usr")[1:2], las=1,xaxs="i",yaxs="i",col="black",pch=19,cex=1.3,axes=F,ylab="",xlab="")
      box(col=subtle.color,lwd=0.5)
      x.pos <- par("usr")[1] + 0.95*diff(par("usr")[1:2])   #par("usr") spits out the current coordinates of the plot window
      y.pos <- par("usr")[3] + 0.8*diff(par("usr")[3:4])   #par("usr") spits out the current coordinates of the plot window
      text(x=x.pos,y=y.pos,years[yr],cex=1.1, col=subtle.color)
      par(xpd=T)
   }
   mtext(side=1,outer=T,"Age",line=2.5)
   mtext(side=2,outer=T,"Proportion",line=3.5)
   mtext(side=3,outer=T,line=1.2,"Indonesian age composition data")
   mtext(side=3,outer=T,line=0.2,paste("(",case_label,")",sep=""),cex=0.6)
}