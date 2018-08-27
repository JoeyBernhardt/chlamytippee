
library(tidyverse)
library(plotrix)
library(readxl)
library(janitor)
library(cowplot)


day2_33perc <- read_csv("data-processed/CT-pilot-day2-33percent.csv")
dilutions <- read_excel("data/dilutions-aug-21-2018.xlsx") %>% 
	clean_names()



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
