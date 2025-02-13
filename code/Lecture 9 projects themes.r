####################################################################
#Lecture on complex projects and custom
#themes for FISH554, Beautiful Graphics in R
#Trevor Branch, tbranch@uw.edu 
####################################################################
require(tidyverse)
require(PBSmodelling)

#====using grep to simplify file management=========================
#when a file's column headings change from year to year so you 
#can't just specify the column numbers
#===================================================================
x <- read.table(file="projections.s4", skip=2, header=T)
colnames(x)
grep("Rec.", colnames(x))
Y <- grep("Rec.", colnames(x))
rec.data <- x[,Y]
rec.data
rec.quants <- apply(rec.data,MARGIN=2,quantile, c(0.05,0.5,0.95))
rec.quants <- data.frame(quantiles=c("5%", "95%"), rec.quants)
rec.long <- pivot_longer(data=rec.quants, cols=-1) #exclude first col

#ugly figure... 
ggplot(data = rec.long, mapping=aes(x=name, y=value)) + 
   geom_line() + theme_minimal()

#=================complex data with PBSmodelling==========
#The PBSmodelling package contains commands writeList and readList
#that are very useful. 
#Open output1.rep and look at the information in it. 
#=========================================================
require(PBSmodelling)
x <- readList(fname="output1.rep")
x$yrs.cpue
x$cpue
x$cpue.pred
cpue.data <- data.frame(Years=x$yrs.cpue[1]:x$yrs.cpue[2],
                        CPUE.obs=x$cpue,
                        CPUE.pred=x$cpue.pred)
cpueplot <- ggplot(data=cpue.data, aes(x=Years, y=CPUE.obs)) + 
   geom_point(size=2, col="blue") + theme_minimal() +
   geom_line(aes(x=Years, y=CPUE.pred)) +
   ylab("CPUE")
cpueplot

#================writing a list to a nice looking file
#using PBSmodeling command writeList
#===============================================================================
#nice looking format, easy to read, with format="P"
writeList(fname="writeList.dat", x=x, format="P")  

#======using functional code with the source() command==========================
#Shows how to create multiple figures by sourcing a plotting function
#then calling it, imagining doing this in three successive years
#===============================================================================
require(PBSmodelling)
require(gplots)

source("IndoAgeFits.r")  #makes the code in this file available
x <- readList(fname="output1.rep")  #data for 2024
IndoAgeFits(data.object=x, case_label="2024 FISH 554 class example")
x <- readList(fname="output1.rep")  #data for 2025 
IndoAgeFits(data.object=x, case_label="2025 FISH 554 class example")
x <- readList(fname="output1.rep")  #data for 2026
IndoAgeFits(data.object=x, case_label="2026 FISH 554 class example")

#=========custom themes=========================================================
#Creating custom themes of your own
#code from Sean Anderson for theme_sleek() available on his github
#https://github.com/seananderson/ggsidekick
#this is how to install a package that is not present in CRAN
#require(devtools) #takes time to download
#require(Rtools)   #does not work on latest version of R
#devtools::install_github("seananderson/ggsidekick")
#===============================================================================
source("theme_sleek.r")
p <- ggplot2::ggplot(mtcars) +
   ggplot2::geom_point(ggplot2::aes(x = wt, y = mpg, colour = factor(gear))) +
   ggplot2::facet_wrap(~am)
p
p + theme_sleek()

#I created a custom theme with particularly awful defaults. 
source("theme_554.r")
p + theme_554(base_size = 100)
