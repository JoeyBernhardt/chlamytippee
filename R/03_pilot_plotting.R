

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
innocs_with_zeros <- read_csv("data-processed/pilot_innoculation_densities_zeros.csv") %>% 
	rename(photo_name = Slice)


innoculation_densities2 <- innoculation_densities %>% 
	separate(file_name, into = c("file_path", "photo_name"), sep = "ws_") %>% 
	mutate(photo_name = str_replace(photo_name, "_results", ""))


innoculation_densities3 <- left_join(innoculation_densities2, innocs_with_zeros, by = "photo_name") 

### new innocs with now accounting for the zeros
innocs <- innoculation_densities3 %>% 
	mutate(cell_id2 = ifelse(Count %in% c(0, 1), 0, cell_id)) %>% 
	group_by(well, extra_photo_info) %>% 
	mutate(cell_count = max(cell_id2)) 


### bring in plate layout

plate <- read_excel("data/plate_layout_aug-21-2018.xlsx") %>% 
	rename(row = X__1)
write_csv(plate, "data-processed/plate_pilot_aug21.csv")
plate_pilot <- read_plate(file = "data-processed/plate_pilot_aug21.csv", well_ids_column = "well") %>% 
	rename(population_density = row) %>% 
	mutate(well = str_replace(well, "0", ""))

write_csv(plate_pilot, "data-processed/plate-pilot.csv")

innocs2 <- left_join(innocs, plate_pilot, by = "well")


dilutions <- read_excel("data/dilutions-aug-21-2018.xlsx") %>% 
	clean_names()

innocs3 <- left_join(innocs2, dilutions, by = "population_density")

innocs3 %>% 
	ungroup() %>% 
	distinct(file_name) %>% 
	tally() 

innocs3 %>% 
	# filter(cell_count < 400, percent_of_stock > 0.001) %>% 
	group_by(well, percent_of_stock) %>% 
	mutate(cell_count_per_ml = (cell_count*206.5997)/4*10) %>% 
	summarise_each(funs(mean, std.error), cell_count_per_ml) %>% 
	filter(cell_count_per_ml_mean < 500000) %>% 
	ggplot(aes(x = percent_of_stock, y = cell_count_per_ml_mean)) + geom_point() +
	geom_errorbar(aes(ymin = cell_count_per_ml_mean - cell_count_per_ml_std.error, ymax = cell_count_per_ml_mean + cell_count_per_ml_std.error), width = 0.01)
ggsave("figures/cell_counts_innoc.pdf", width = 6, height = 5)


innocs4 <- innocs3 %>% 
	# filter(well == "E3") %>% 
	mutate(cell_count = ifelse(photo_name == "180822_142908_E3_Phase_10x_(5).jpg", Count, cell_count)) %>% 
	mutate(cell_count = ifelse(percent_of_stock < 0.2, Count, cell_count)) %>% 
	group_by(well, percent_of_stock) %>% 
	mutate(cell_count_per_ml = (cell_count*206.5997)/4*10*3.5) %>% 
	summarise_each(funs(mean, std.error), cell_count_per_ml) 
write_csv(innocs4, "data-processed/ct-pilot-innoc-cell-counts.csv")

innocs4 %>% 
	ggplot(aes(x = percent_of_stock, y = cell_count_per_ml_mean)) + geom_point() +
	geom_errorbar(aes(ymin = cell_count_per_ml_mean - cell_count_per_ml_std.error, ymax = cell_count_per_ml_mean + cell_count_per_ml_std.error), width = 0.01)
ggsave("figures/cell_counts_innoc.pdf", width = 6, height = 5)

