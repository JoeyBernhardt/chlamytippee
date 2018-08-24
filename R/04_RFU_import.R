

library(tidyverse)
library(readxl)
library(plater)
library(stringr)

dilutions <- read_excel("data/dilutions-aug-21-2018.xlsx") %>% 
	clean_names()

plate_pilot <- read_plate(file = "data-processed/plate_pilot_aug21.csv", well_ids_column = "well") %>% 
	rename(population_density = row) %>% 
	mutate(well = str_replace(well, "0", ""))

plate1 <- read_excel("data/RFU-2018-08-23-2shakes/plate1.xlsx", skip = 47) %>% 
	select(-X__2) %>% 
	rename(row = X__1)
write_csv(plate1, "data-processed/plate1-CT-pilot-day2.csv")
plate2 <- read_excel("data/RFU-2018-08-23-2shakes/plate2.xlsx", skip = 47) %>% 
	select(-X__2) %>% 
	rename(row = X__1)
write_csv(plate2, "data-processed/plate2-CT-pilot-day2.csv")

plate3 <- read_excel("data/RFU-2018-08-23-2shakes/plate3.xlsx", skip = 47) %>% 
	select(-X__2) %>% 
	rename(row = X__1)
write_csv(plate3, "data-processed/plate3-CT-pilot-day2.csv")

plate4 <- read_excel("data/RFU-2018-08-23-2shakes/plate4.xlsx", skip = 47) %>% 
	select(-X__2) %>% 
	rename(row = X__1)
write_csv(plate4, "data-processed/plate4-CT-pilot-day2.csv")


plate5 <- read_excel("data/RFU-2018-08-23-2shakes/plate5.xlsx", skip = 47) %>% 
	select(-X__2) %>% 
	rename(row = X__1)
write_csv(plate5, "data-processed/plate5-CT-pilot-day2.csv")



# Bring em in -------------------------------------------------------------

plate1df <- read_plate(file = "data-processed/plate1-CT-pilot-day2.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate1") %>% 
	mutate(well = str_replace(well, "0", "")) %>% 
	rename(RFU = row) %>% 
	mutate(light = 50)

plate2df <- read_plate(file = "data-processed/plate2-CT-pilot-day2.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate2") %>% 
	mutate(well = str_replace(well, "0", "")) %>% 
	rename(RFU = row) %>% 
	mutate(light = 33)

plate3df <- read_plate(file = "data-processed/plate3-CT-pilot-day2.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate3") %>% 
	mutate(well = str_replace(well, "0", "")) %>% 
	rename(RFU = row) %>% 
	mutate(light = 70)

plate4df <- read_plate(file = "data-processed/plate4-CT-pilot-day2.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate4") %>% 
	mutate(well = str_replace(well, "0", "")) %>% 
	rename(RFU = row) %>% 
	mutate(light = 100)


plate5df <- read_plate(file = "data-processed/plate5-CT-pilot-day2.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate5") %>% 
	mutate(well = str_replace(well, "0", "")) %>% 
	rename(RFU = row) %>% 
	mutate(light = 5)


all_RFUs <- bind_rows(plate1df, plate2df, plate3df, plate4df, plate5df)

RFU_all <- left_join(all_RFUs, plate_pilot, by = "well")
RFU_all2 <- left_join(RFU_all, dilutions, by = "population_density") %>% 
	mutate(light_photons = light/100*700)

RFU_all2 %>% 
	ggplot(aes(x = percent_of_stock, y = RFU)) + geom_point() +
	facet_wrap( ~ light)

ggplot(data = RFU_all2, aes(x=light_photons, y=percent_of_stock, color = RFU)) + 
	geom_point(shape = 15, size = 10) + scale_color_viridis_c() +
	ylab("Population density") + xlab("Irradiance (umol/m2/s)")
ggsave("figures/RFU-day-2-heatmap.pdf", width = 5, height = 4)

RFU_all2 %>% 
	ggplot(aes(x = light_photons, y = RFU)) + geom_point(size = 3) +
	facet_wrap( ~ percent_of_stock, scales = "free") + xlab("Irradiance (umol/m2/s)") +ggtitle("RFU-Irradiance plots at different starting population densities")
ggsave("figures/RFU-irradiance-day-2.pdf", width = 8, height = 6)
# alternative import ------------------------------------------------------



RFU_files <- c(list.files("data/RFU-2018-08-23-2shakes", full.names = TRUE))



names(RFU_files) <- RFU_files %>% 
	gsub(pattern = ".xlsx$", replacement = "")

all_plates <- map_df(RFU_files, read_excel, skip = 47, .id = "file_name") %>% 
	select(-X__2) %>% 
	rename(row = X__1)
plate1 <- all_plates %>% 
	filter(grepl("plate1", file_name)) %>% 
	select(-file_name)
