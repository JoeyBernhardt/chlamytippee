

library(tidyverse)
library(readxl)
library(plater)
library(janitor)
library(cowplot)
library(plotrix)

innoculation_densities <- read_csv("data-processed/pilot_innoculation_densities.csv")

innocs <- innoculation_densities %>% 
	group_by(well, extra_photo_info) %>% 
	mutate(cell_count = max(cell_id)) 


### bring in plate layout

plate <- read_excel("data/plate_layout_aug-21-2018.xlsx") %>% 
	rename(row = X__1)
write_csv(plate, "data-processed/plate_pilot_aug21.csv")
plate_pilot <- read_plate(file = "data-processed/plate_pilot_aug21.csv", well_ids_column = "well") %>% 
	rename(population_density = row) %>% 
	mutate(well = str_replace(well, "0", ""))


innocs2 <- left_join(innocs, plate_pilot, by = "well")


dilutions <- read_excel("data/dilutions-aug-21-2018.xlsx") %>% 
	clean_names()

innocs3 <- left_join(innocs2, dilutions, by = "population_density")

innocs3 %>% 
	ungroup() %>% 
	distinct(file_name) %>% 
	tally() 
innocs3 %>% 
	filter(cell_count < 400, percent_of_stock > 0.001) %>% 
	group_by(well, percent_of_stock) %>% 
	summarise_each(funs(mean, std.error), cell_count) %>% 
	ggplot(aes(x = percent_of_stock, y = cell_count_mean)) + geom_point() +
	geom_errorbar(aes(ymin = cell_count_mean - cell_count_std.error, ymax = cell_count_mean + cell_count_std.error), width = 0.01)
ggsave("figures/cell_counts_innoc.pdf", width = 6, height = 5)
