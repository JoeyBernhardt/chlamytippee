

### import cell counts and sizes
library(tidyverse)
library(stringr)




cell_files <- c(list.files("imageJ-results", full.names = TRUE))
cell_files_txt <- cell_files[grepl(".txt", cell_files)]


names(cell_files_txt) <- cell_files_txt %>% 
	gsub(pattern = ".txt$", replacement = "")


#### Step 3: read in all the files!

all_cells <- map_df(cell_files_txt, read_tsv, col_names = TRUE, .id = "file_name")

all_cells2 <- all_cells %>% 
	filter(!grepl("test", file_name)) %>% 
	filter(grepl("Phase", file_name)) %>% 
	rename(cell_id = X1) %>% 
	mutate(well = file_name) %>%
	separate(well, into = c("extra", "well"), sep = 42) %>% 
	separate(well, into = c("well", "extra_photo_info"), sep = 2)


write_csv(all_cells2, "data-processed/pilot_innoculation_densities.csv")



# Day 2 33 percent --------------------------------------------------------


cell_files <- c(list.files("imageJ-results/2018-08-27-CT-day2-33percent-brightfield", full.names = TRUE))
cell_files_txt <- cell_files[grepl(".txt", cell_files)]


names(cell_files_txt) <- cell_files_txt %>% 
	gsub(pattern = ".txt$", replacement = "")


#### Step 3: read in all the files!

all_cells <- map_df(cell_files_txt, read_tsv, col_names = TRUE, .id = "file_name")

all_cells2 <- all_cells %>% 
	rename(cell_id = X1) %>% 
	mutate(well = file_name) %>%
	separate(well, into = c("extra", "well"), sep = "ws_") %>% 
	separate(well, into = c("well", "extra_photo_info"), sep = 2)


write_csv(all_cells2, "data-processed/CT-pilot-day2-33percent.csv")


# 96 well import --------------------------------------------------------


cell_files <- c(list.files("/Users/joeybernhardt/Documents/96-well-results", full.names = TRUE))
cell_files_txt <- cell_files[grepl(".txt", cell_files)]


names(cell_files_txt) <- cell_files_txt %>% 
	gsub(pattern = ".txt$", replacement = "")


#### Step 3: read in all the files!

all_cells <- map_df(cell_files_txt, read_tsv, col_names = TRUE, .id = "file_name")

all_cells$file_name

all_cells2 <- all_cells %>% 
	rename(cell_id = X1) %>% 
	mutate(well = file_name) %>% 
	separate(well, into = c("extra", "well"), sep = "ws_") %>% 
	filter(grepl("no-auto-expose", well)) %>% 
	mutate(well = str_replace(well, "-round2", "")) %>%
	separate(well, into = c("extra2", "well"), sep = "expose_") %>%
	separate(well, into = c("well", "extra_photo_info"), sep = 2) %>% 
	filter(!is.na(well)) %>% 
	filter(cell_id < 7000)


write_csv(all_cells2, "data-processed/CT-pilot-96-well-data.csv")

