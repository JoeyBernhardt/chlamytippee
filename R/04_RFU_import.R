

library(tidyverse)
library(readxl)
library(plater)
library(stringr)
library(janitor)
library(lubridate)
library(cowplot)
library(broom)

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




# day 4 data --------------------------------------------------------------

plate1 <- read_excel("data/RFU-2018-08-25-2shakes/plate1.xlsx", skip = 47) %>% 
	select(-X__2) %>% 
	rename(row = X__1)
write_csv(plate1, "data-processed/plate1-CT-pilot-day4.csv")
plate2 <- read_excel("data/RFU-2018-08-25-2shakes/plate2.xlsx", skip = 47) %>% 
	select(-X__2) %>% 
	rename(row = X__1)
write_csv(plate2, "data-processed/plate2-CT-pilot-day4.csv")

plate3 <- read_excel("data/RFU-2018-08-25-2shakes/plate3.xlsx", skip = 47) %>% 
	select(-X__2) %>% 
	rename(row = X__1)
write_csv(plate3, "data-processed/plate3-CT-pilot-day4.csv")

plate4 <- read_excel("data/RFU-2018-08-25-2shakes/plate4.xlsx", skip = 47) %>% 
	select(-X__2) %>% 
	rename(row = X__1)
write_csv(plate4, "data-processed/plate4-CT-pilot-day4.csv")


plate5 <- read_excel("data/RFU-2018-08-25-2shakes/plate5.xlsx", skip = 47) %>% 
	select(-X__2) %>% 
	rename(row = X__1)
write_csv(plate5, "data-processed/plate5-CT-pilot-day4.csv")



# Bring em in -------------------------------------------------------------

plate1df <- read_plate(file = "data-processed/plate1-CT-pilot-day2.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate1") %>% 
	mutate(well = str_replace(well, "0", "")) %>% 
	rename(RFU = row) %>% 
	mutate(light = 50) %>% 
	mutate(date = ymd("2018-08-23"))

plate2df <- read_plate(file = "data-processed/plate2-CT-pilot-day2.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate2") %>% 
	mutate(well = str_replace(well, "0", "")) %>% 
	rename(RFU = row) %>% 
	mutate(light = 33) %>% 
	mutate(date = ymd("2018-08-23"))

plate3df <- read_plate(file = "data-processed/plate3-CT-pilot-day2.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate3") %>% 
	mutate(well = str_replace(well, "0", "")) %>% 
	rename(RFU = row) %>% 
	mutate(light = 70)  %>% 
	mutate(date = ymd("2018-08-23"))

plate4df <- read_plate(file = "data-processed/plate4-CT-pilot-day2.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate4") %>% 
	mutate(well = str_replace(well, "0", "")) %>% 
	rename(RFU = row) %>% 
	mutate(light = 100)  %>% 
	mutate(date = ymd("2018-08-23"))


plate5df <- read_plate(file = "data-processed/plate5-CT-pilot-day2.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate5") %>% 
	mutate(well = str_replace(well, "0", "")) %>% 
	rename(RFU = row) %>% 
	mutate(light = 5)  %>% 
	mutate(date = ymd("2018-08-23"))


# Bring em in day 4 -------------------------------------------------------------

plate1df_day4 <- read_plate(file = "data-processed/plate1-CT-pilot-day4.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate1") %>% 
	mutate(well = str_replace(well, "0", "")) %>% 
	rename(RFU = row) %>% 
	mutate(light = 50) %>% 
	mutate(date = ymd("2018-08-25"))

plate2df_day4 <- read_plate(file = "data-processed/plate2-CT-pilot-day4.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate2") %>% 
	mutate(well = str_replace(well, "0", "")) %>% 
	rename(RFU = row) %>% 
	mutate(light = 33) %>% 
	mutate(date = ymd("2018-08-25"))

plate3df_day4 <- read_plate(file = "data-processed/plate3-CT-pilot-day4.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate3") %>% 
	mutate(well = str_replace(well, "0", "")) %>% 
	rename(RFU = row) %>% 
	mutate(light = 70)  %>% 
	mutate(date = ymd("2018-08-25"))

plate4df_day4 <- read_plate(file = "data-processed/plate4-CT-pilot-day4.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate4") %>% 
	mutate(well = str_replace(well, "0", "")) %>% 
	rename(RFU = row) %>% 
	mutate(light = 100)  %>% 
	mutate(date = ymd("2018-08-25"))


plate5df_day4 <- read_plate(file = "data-processed/plate5-CT-pilot-day4.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate5") %>% 
	mutate(well = str_replace(well, "0", "")) %>% 
	rename(RFU = row) %>% 
	mutate(light = 5)  %>% 
	mutate(date = ymd("2018-08-25"))

all_RFUs <- bind_rows(plate1df, plate2df, plate3df, plate4df, plate5df, 
					  plate1df_day4, plate2df_day4, plate3df_day4, plate4df_day4, plate5df_day4)

RFU_all <- left_join(all_RFUs, plate_pilot, by = "well")
RFU_all2 <- left_join(RFU_all, dilutions, by = "population_density") %>% 
	mutate(light_photons = light/100*700)
write_csv(RFU_all2, "data-processed/rfu-day2-4.csv")

RFU_all2 <- read_csv("data-processed/rfu-day2-4.csv")

RFU_all2 %>% 
	ggplot(aes(x = date, y = RFU, color = percent_of_stock)) + geom_point() +
	facet_wrap( ~ percent_of_stock) + geom_smooth(method = "lm")



ggplot(data = RFU_all2, aes(x=light_photons, y=percent_of_stock, color = RFU)) + 
	geom_point(shape = 15, size = 10) + scale_color_viridis_c() +
	ylab("Population density") + xlab("Irradiance (umol/m2/s)")
ggsave("figures/RFU-day-2-heatmap.pdf", width = 5, height = 4)

RFU_all2 %>% 
	# filter(date == "2018-08-25") %>% 
ggplot(aes(x=light_photons, y=percent_of_stock, color = RFU)) + 
	geom_point(shape = 15, size = 10) + scale_color_viridis_c() +
	ylab("Population density") + xlab("Irradiance (umol/m2/s)") + 
	facet_wrap( ~ date)
ggsave("figures/RFU-day-2-4-heatmap.pdf", width = 7, height = 4)


RFU_all2 %>% 
	ggplot(aes(x = light_photons, y = RFU, color = factor(date))) + geom_point(size = 3) +
	facet_wrap( ~ percent_of_stock, scales = "free") +
	xlab("Irradiance (umol/m2/s)") +
	ggtitle("RFU-Irradiance plots at different starting population densities") +
	scale_color_viridis_d(begin = 0.7, end = 0.3)
ggsave("figures/RFU-irradiance-day-2-4.pdf", width = 10, height = 6)


# now let’s get change in RFU --------------------------------------------

RFU_all2 %>% 
	spread(key = date, value = RFU) %>% 
	mutate(delta_RFU = (`2018-08-25`-`2018-08-23`)/ `2018-08-23`)%>%
	group_by(light_photons, percent_of_stock) %>% 
	summarise_each(funs(mean), delta_RFU) %>% 
	mutate(growth_rate = ifelse(delta_RFU > 0, "positive", "negative")) %>% 
	ggplot(aes(x=light_photons, y=percent_of_stock, color = growth_rate)) + 
	geom_point(shape = 15, size = 10) + 
	# scale_color_brewer(type = "div") +
	# scale_color_gradient2(low = "blue", high = "red", mid = "white", 
				# midpoint = 0, space = "Lab") +
	ylab("Population density") + xlab("Irradiance (umol/m2/s)")
ggsave("figures/RFU-heatmap-growth-rate.pdf", width = 7, height = 6)
	

slopes <- RFU_all2 %>% 
	mutate(days = ifelse(date == "2018-08-25", 4, 2)) %>% 
	group_by(light_photons, percent_of_stock) %>% 
	do(tidy(lm(log(RFU+1) ~ days, data = .), conf.int = TRUE)) %>% 
	filter(term == "days") %>% 
	ungroup()


slopes %>% 
	mutate(growth_rate = ifelse(estimate > 0, "positive", "negative")) %>% 
	ggplot(aes(x=light_photons, y=percent_of_stock, color = estimate)) + 
	geom_point(shape = 15, size = 10) + 
	scale_color_brewer(type = "div") +
	scale_color_gradient2(low = "blue", high = "red", mid = "white",
	midpoint = 0, space = "Lab", name = "Growth rate") +
	ylab("Population density") + xlab("Irradiance (umol/m2/s)")
ggsave("figures/RFU-growth-heatmap.pdf", width = 7, height = 4)

slopes %>% 
	mutate(growth_rate = ifelse(estimate > 0, "positive", "negative")) %>% 
	ggplot(aes(x=light_photons, y=percent_of_stock, color = growth_rate)) + 
	geom_point(shape = 15, size = 10) + 
	# scale_color_brewer(type = "div") +
	# scale_color_gradient2(low = "blue", high = "red", mid = "white", 
	# midpoint = 0, space = "Lab") +
	ylab("Population density") + xlab("Irradiance (umol/m2/s)")




RFU_all2 %>% 
	mutate(days = ifelse(date == "2018-08-25", 4, 2)) %>% 
	ggplot(aes(x = days, y = log(RFU+1))) + geom_point() +
	facet_wrap( ~ percent_of_stock + light_photons, nrow = 8, ncol = 5) +
	geom_smooth(method = "lm", color = "black")
ggsave("figures/RFU_slopes.pdf", width = 10, height = 12)


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




# bring in 96 well plate pilot --------------------------------------------

aug_28 <- read_excel("data/RFU-96-well-pilot/RFU-2018-08-28-96well-growth-pilot.xlsx", skip = 50) %>% 
	select(-X__2) %>% 
	rename(row = X__1)
write_csv(aug_28, "data-processed/96well-growth-pilot-aug28.csv")

aug_29 <- read_excel("data/RFU-96-well-pilot/RFU-2018-08-29-96well-growth-pilot.xlsx", skip = 50) %>% 
	select(-X__2) %>% 
	rename(row = X__1)
write_csv(aug_29, "data-processed/96well-growth-pilot-aug29.csv")

aug_30 <- read_excel("data/RFU-96-well-pilot/RFU-2018-08-30-96well-growth-pilot.xlsx", skip = 50) %>% 
	select(-X__2) %>% 
	rename(row = X__1)
write_csv(aug_30, "data-processed/96well-growth-pilot-aug30.csv")

aug_31 <- read_excel("data/RFU-96-well-pilot/RFU-2018-08-31-96well-growth-pilot.xlsx", skip = 50) %>% 
	select(-X__2) %>% 
	rename(row = X__1)
write_csv(aug_31, "data-processed/96well-growth-pilot-aug31.csv")

sep_03 <- read_excel("data/RFU-96-well-pilot/RFU-2018-09-03-96well-growth-pilot.xlsx", skip = 50) %>% 
	select(-X__2) %>% 
	rename(row = X__1)
write_csv(sep_03, "data-processed/96well-growth-pilot-sep03.csv")

aug_28_RFU <- read_plate(file = "data-processed/96well-growth-pilot-aug28.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate1") %>% 
	rename(RFU = row) %>% 
	mutate(date = ymd("2018-08-28"))

aug_29_RFU <- read_plate(file = "data-processed/96well-growth-pilot-aug29.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate1") %>% 
	rename(RFU = row) %>% 
	mutate(date = ymd("2018-08-29"))


aug_30_RFU <- read_plate(file = "data-processed/96well-growth-pilot-aug30.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate1") %>% 
	rename(RFU = row) %>% 
	mutate(date = ymd("2018-08-30"))

aug_31_RFU <- read_plate(file = "data-processed/96well-growth-pilot-aug31.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate1") %>% 
	rename(RFU = row) %>% 
	mutate(date = ymd("2018-08-31"))

sep_03_RFU <- read_plate(file = "data-processed/96well-growth-pilot-sep03.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate1") %>% 
	rename(RFU = row) %>% 
	mutate(date = ymd("2018-09-03"))

aug_31_RFU <- read_plate(file = "data-processed/96well-growth-pilot-aug31.csv", well_ids_column = "well") %>% 
	mutate(plate = "plate1") %>% 
	rename(RFU = row) %>% 
	mutate(date = ymd("2018-08-31"))

all_growth_pilot <- bind_rows(aug_28_RFU, aug_29_RFU, aug_30_RFU, aug_31_RFU, sep_03_RFU) %>% 
	mutate(volume = ifelse(grepl("D|G", well), 200, 100)) %>% 
	mutate(volume = factor(volume)) %>% 
	filter(!well %in% c("H04", "H05", "H06", "H07", "H08", "H09", "H10", "H11", "H12"))

all_growth_pilot %>% 
	ggplot(aes(x = date, y = RFU, color = volume, group = well)) + geom_point() +
	facet_wrap( ~ volume) + geom_line() + scale_color_viridis_d(end = 0.5)
ggsave("figures/96-well-pilot-RFU.pdf", width = 10, height = 5)






