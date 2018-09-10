
library(flowCore)
library(tidyverse)
library(cowplot)

b2 <- read.FCS("data/flow-cytometry-eawag/20180830_121123/B02.fcs")


library('flowCore')
library('gplots')

#### parameters ####

f.name <- 'data/flow-cytometry-eawag/20180830_121123/B02.fcs'  # name of the file you want to analyze, file must have extension ".FCS"
sample.size <- max               # number of events to plot, use "max" for all points
fsc.ll <- 1                      # FSC lower limit
ssc.ll <- 1                      # SSC lower limit
fl1.ll <- 1                      # FL1 lower limit (ex488/em536)


#### read in file ####
library(janitor)
fcm_raw <- read.FCS("data/flow-cytometry-eawag/20180830_121123/B05.fcs")
fcm <- as.data.frame((exprs(fcm_raw))) %>% 
	clean_names()

### let's read in all of the fcs files
fcs_files <- c(list.files("data/flow-cytometry-eawag/20180830_121123", full.names = TRUE))

names(fcs_files) <- fcs_files %>% 
	gsub(pattern = ".fcs$", replacement = "")


#### Step 3: read in all the files!


read_fcs <- function(file) {
	fcs_file <- as.data.frame((exprs(read.FCS(file)))) %>% 
		clean_names()
	
}
all_fcs <- map_df(fcs_files, read_fcs, .id = "file_name")


all_fcs2 <- all_fcs %>% 
	separate(file_name, into = c("file_path", "well"), sep = -3, remove = FALSE)


colfunc <- colorRampPalette(c("white", "lightblue", "green", "yellow", "red"))


all_fcs2 %>% 
	# filter(well == "B05", fl1_a > 0) %>% 
ggplot(aes(x=log(fsc_a), y=log(fl1_a))) +
	# ylim(0, 30000) +
	# xlim(0,50000) +
	stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) +
	scale_fill_gradientn(colours=colfunc(100)) + 
	geom_density2d(colour="black", bins=4) +
	facet_wrap( ~ well)


library(cowplot)
fcm %>% 
	ggplot(aes(x = fsc_a, y = fl1_a)) + geom_point() + scale_y_log10() + scale_x_log10()

fcm %>% 
	filter(fl1_a > 0) %>% 
	ggplot(aes(x = fl1_a)) + geom_histogram()
fcm %>% 
	# filter(fl1_a > 0) %>% 
	ggplot(aes(x = fsc_a)) + geom_histogram()



# this colour palette can be changed to your taste 

ggplot(fcm, aes(x=fsc_a, y=fl1_a)) +
	ylim(0, 30000) +
	# xlim(0,50000) +
	stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) +
	scale_fill_gradientn(colours=colfunc(400)) + # gives the colour plot
	geom_density2d(colour="black", bins=5) # draws the lines inside


