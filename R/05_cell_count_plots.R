
library(tidyverse)
library(plotrix)
library(readxl)
library(janitor)
library(cowplot)
library(stringr)
library(broom)


day2_33perc <- read_csv("data-processed/CT-pilot-day2-33percent.csv")

well_96_data <- read_csv("~/Desktop/CT-pilot-96-well-data.csv") %>% 
	filter(Area < 200) %>% 
	mutate(day = NA) %>% 
	mutate(day = ifelse(grepl("day2", extra2), "day2", "day4"))


well_96_data_2$photo_name[[1]]
zero_photos$Slice[[1]]
well_96_data_2 <- well_96_data %>% 
	separate(file_name, into = c("file_path", "photo_name"), sep = "ws_") %>% 
	mutate(photo_name = str_replace(photo_name, "_results", ""))



# ok let’s get rid of the photos here with no cells -----------------------

zero_photo_names <- zero_photos %>% 
	filter(Count == 0) %>% 
	select(Slice)

well_96_data_3 <- well_96_data_2 %>% 
	filter(!photo_name %in% c(zero_photo_names$Slice))



dilutions <- read_excel("data/dilutions-aug-21-2018.xlsx") %>% 
	clean_names()

zero_photos <- read_csv("data-processed/pilot-96-well-finding-zeros.csv") %>% 
	mutate(photo_name = Slice)

plate_pilot <- read_csv("data-processed/plate-pilot.csv")

day2_densities <- day2_33perc %>% 
	group_by(well, extra_photo_info) %>% 
	mutate(cell_count = max(cell_id))



day2_densities2 <- left_join(day2_densities, plate_pilot, by = "well") %>% 
	mutate(light = "33") %>% 
	mutate(date = "2018-08-23")


day2_summ <- day2_densities2 %>%
	group_by(well, population_density) %>% 
	summarise_each(funs(mean, std.error), cell_count) %>%  
	distinct(file_name, .keep_all = TRUE) %>% 
	filter(cell_count_mean < 5000)


day2_densities2 %>%
	filter(cell_count > 5000) %>% 
	ungroup() %>% 
	distinct(well)

day2_summ_2 <- left_join(day2_summ, dilutions, by = "population_density")

day2_summ_2 %>% 
	ggplot(aes(x = percent_of_stock, y = cell_count_mean)) + geom_point() +
	geom_errorbar(aes(ymin = cell_count_mean - cell_count_std.error, ymax = cell_count_mean + cell_count_std.error), width = 0.05) +
	ylab("Cell count") + xlab("Starting population density (percent)")
ggsave("figures/day2-33-percent-cell-counts.pdf", width = 6, height = 5)


# bring in RFUs -----------------------------------------------------------

RFUs <- read_csv("data-processed/rfu-day2-4.csv") %>% 
	filter(date == "2018-08-23") %>% 
	filter(light == "33")

rfu_densities <- left_join(day2_summ, RFUs, by = c("well", "population_density"))

rfu_densities %>% 
	filter(cell_count_mean <1000) %>% 
	ungroup() %>% 
	ggplot(aes(x = cell_count_mean, y = RFU, color = )) + geom_point() +
	geom_abline(slope = 1, intercept = 0) +
	ylab("RFU") + xlab("Cell count") + geom_smooth(method = "lm", color = "black")
ggsave("figures/cell_count_RFU.pdf", width = 5, height =4)

library(broom)
rfu_densities %>% 
	filter(cell_count_mean <1000) %>% 
	ungroup() %>% 
	lm(RFU ~ cell_count_mean, data = .) %>% 
	tidy(., conf.int = TRUE)



# 96 well plate data ------------------------------------------------------

well_96_data_4 <- left_join(well_96_data_2, zero_photos, by = "photo_name") 

densities <- well_96_data_4 %>% 
	mutate(cell_id2 = ifelse(Count == 0, 0, cell_id)) %>% 
	group_by(well, extra2, day) %>% 
	mutate(cell_count = max(cell_id2)) %>% 
	mutate(light_level = NA) %>% 
	mutate(light_level = case_when(grepl("05percent", extra2) ~ "5",
								   grepl("5percent", extra2) ~ "5",
								   grepl("70percent", extra2) ~ "70",
								   grepl("50percent", extra2) ~ "50",
								   grepl("100percent", extra2) ~ "100",
								   grepl("33percent", extra2) ~ "33")) 


dens_sum <- densities %>% 
	group_by(well, light_level, day) %>% 
	summarise_each(funs(mean), cell_count) 

dens_sum_m <- densities %>% 
	group_by(well, light_level, day) %>% 
	summarise_each(funs(mean), Count) 




dens_sum2 <- left_join(dens_sum, plate_pilot, by = "well")
dens_sum3 <- left_join(dens_sum2, dilutions, by = "population_density")


write_csv(dens_sum3, "data-processed/96-well-cell-counts.csv")

innoc_dens <- read_csv("data-processed/ct-pilot-innoc-cell-counts.csv") %>% 
	mutate(day = "day0")

all_counts <- left_join(dens_sum3, innoc_dens, by = "well")
slopes <- all_counts %>% 
	ungroup() %>% 
	mutate(days = case_when(day == "day4" ~ 4,
							day == "day0" ~ 0,
							day == "day2" ~ 2)) %>% 
	mutate(light_level = as.numeric(light_level)) %>% 
	mutate(light_photons = 700*(light_level/100)) %>% 
	group_by(light_photons, percent_of_stock) %>% 
	do(tidy(lm(log(cell_count+1) ~ days, data = .), conf.int = TRUE)) %>% 
	filter(term == "days") %>% 
	ungroup() 

dens_sum3 %>% 
	ungroup() %>% 
	mutate(days = ifelse(day == "day4", 4, 2)) %>% 
	mutate(light_level = as.numeric(light_level)) %>% 
	mutate(light_photons = 700*(light_level/100)) %>% 
	ggplot(aes(x = days, y = log(cell_count+1))) + geom_point() +
	facet_wrap( ~  percent_of_stock + light_photons, nrow = 8, ncol = 5,  scales = "free") + geom_smooth(method = "lm")
ggsave("figures/pilot-cell-counts-days2-4.pdf", width = 10, height = 12)

dens_sum3 %>% 
	ungroup() %>% 
	mutate(days = ifelse(day == "day4", 4, 2)) %>% 
	mutate(light_level = as.numeric(light_level)) %>% 
	mutate(light_photons = 700*(light_level/100)) %>% 
	ggplot(aes(x = days, y = cell_count)) + geom_point() +
	facet_wrap( ~  percent_of_stock + light_photons, nrow = 8, ncol = 5,  scales = "free") + geom_smooth(method = "lm")
ggsave("figures/pilot-cell-counts-days2-4-linear.pdf", width = 10, height = 12)

slopes %>% 
	mutate(growth_rate = ifelse(estimate > 0, "positive", "negative")) %>% 
	ggplot(aes(x=light_photons, y=percent_of_stock, color = estimate)) + 
	geom_point(shape = 15, size = 10) + 
	scale_color_brewer(type = "div") +
	scale_color_gradient2(low = "blue", high = "red", mid = "white",
						  midpoint = 0, space = "Lab", name = "Growth rate") +
	ylab("Population density") + xlab("Irradiance (umol/m2/s)")
ggsave("figures/cell-count-growth-heatmap-zeros.pdf", width = 5, height = 4)

slopes %>% 
	mutate(growth_rate = ifelse(estimate > 0, "positive", "negative")) %>% 
	ggplot(aes(x=light_photons, y=percent_of_stock, color = growth_rate)) + 
	geom_point(shape = 15, size = 10) + 
	scale_color_viridis_d(begin = 0.3, end = 0.9, name = "Growth rate") +
	ylab("Population density") + xlab("Irradiance (umol/m2/s)")
ggsave("figures/cell-count-growth-pos-neg-heatmap-zeros.pdf", width = 5, height = 4)



densities2 <- left_join(day2_densities, plate_pilot, by = "well") %>% 
	mutate(light = "33") %>% 
	mutate(date = "2018-08-23")


day2_summ <- day2_densities2 %>%
	group_by(well, population_density) %>% 
	summarise_each(funs(mean, std.error), cell_count) %>%  
	distinct(file_name, .keep_all = TRUE) %>% 
	filter(cell_count_mean < 5000)
