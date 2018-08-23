

### import cell counts and sizes
library(tidyverse)
library(stringr)

test <- read_tsv("imageJ-results/threshold_ws_B2_-1_1_1_BrightField_001.jpg_results.txt")


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
