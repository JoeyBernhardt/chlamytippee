

source("https://bioconductor.org/biocLite.R")
biocLite("flowCore")


library(flowCore)


b2 <- read.FCS("data/flow-cytometry-eawag/20180830_121123/B02.fcs")


library('flowCore')
library('gplots')

#### parameters ####

f.name <- 'data/flow-cytometry-eawag/20180830_121123/B02.fcs'  # name of the file you want to analyze, file must have extension ".FCS"
sample.size <- max               # number of events to plot, use "max" for all points
fsc.ll <- 1                      # FSC lower limit
ssc.ll <- 1                      # SSC lower limit
fl1.ll <- 1                      # FL1 lower limit (ex488/em536)

#### functions ####

## plotting function

plot.events <- function(fcm, x.param, y.param){
	hist2d(log10(fcm[,x.param]),
		   log10(fcm[,y.param]),
		   col = c('grey', colorRampPalette(c('white', 'lightgoldenrod1', 'darkgreen'))(100)),
		   nbins = 200,
		   bg = 'grey',
		   ylab = paste0('log10(', y.param, ')'),
		   xlab = paste0('log10(', x.param, ')'))
	
	box()
}

#### read in file ####
library(janitor)
fcm_raw <- read.FCS("data/flow-cytometry-eawag/20180830_121123/B05.fcs")
fcm <- as.data.frame((exprs(fcm_raw))) %>% 
	clean_names()

library(cowplot)
fcm %>% 
	ggplot(aes(x = fsc_a, y = fl1_a)) + geom_point() + scale_y_log10() + scale_x_log10()

fcm %>% 
	ggplot(aes(x = fl1_a)) + geom_histogram()

colfunc <- colorRampPalette(c("white", "lightblue", "green", "yellow", "red"))
# this colour palette can be changed to your taste 

ggplot(fcm, aes(x=fsc_a, y=fl1_a)) +
	ylim(0, 30000) +
	# xlim(0,50000) +
	stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) +
	scale_fill_gradientn(colours=colfunc(400)) + # gives the colour plot
	geom_density2d(colour="black", bins=5) # draws the lines inside


#### analyze file and make plot ####

## eliminate values that are below or equal to thresholds you
## defined above

fcm$SSC[fcm$SSC <= ssc.ll|fcm$FSC <= fsc.ll|fcm$FL1 == fl1.ll] <- NA
fcm <- na.omit(fcm)

fcm.sample <- fcm

if(sample.size != 'max'){
	try({fcm.sample <- fcm[sample(length(fcm$SSC), sample.size),]},
		silent = T)
}

## plot events in a couple of different ways

plot.events(fcm, 'FSC', 'SSC')
plot.events(fcm, 'FSC', 'FL1')

## make a presentation quality figure

png(paste0(f.name, '_FSC', '_FL1', '.png'),
	width = 2000,
	height = 2000,
	pointsize = 50)

plot.events(fcm, 'FSC', 'FL1')

dev.off()