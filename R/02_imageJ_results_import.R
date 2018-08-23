

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
	rename(cell_id = X1) %>% 
	mutate(well = file_name) %>%
	mutate(well = str_replace(well, "imageJ-results/threshold_ws_", "")) %>% 
	separate(well, into = c("well", "extra_photo_info"), sep = 2)


