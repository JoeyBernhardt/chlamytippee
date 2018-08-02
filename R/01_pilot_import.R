

library(readxl)
library(tidyverse)
library(plater)
library(lubridate)
library(stringr)
library(cowplot)
library(viridis)


plate <- read_excel("data/Plate Layout 27.07.2018.xlsx") %>% 
rename(row = X__1)
write_csv(plate, "data-processed/plate_pilot.csv")
plate_pilot <- read_plate(file = "data-processed/plate_pilot.csv", well_ids_column = "wells") %>% 
	rename(concentration = row)

day1 <- read_excel("data/Day 1 reads.xlsx", skip = 26) %>% 
	select(-X__2) %>% 
	rename(row = X__1) 
write_csv(day1, "data-processed/pilot-day1.csv")
day1df <- read_plate(file = "data-processed/pilot-day1.csv", well_ids_column = "wells") %>% 
	filter(grepl("A|B", wells)) %>% 
	mutate(day = mdy("7/27/18"))


day3 <- read_excel("data/Day 3 reads.xlsx", skip = 26) %>% 
	select(-X__2) %>% 
	rename(row = X__1)
write_csv(day3, "data-processed/pilot-day3.csv")
day3df <- read_plate(file = "data-processed/pilot-day3.csv", well_ids_column = "wells") %>% 
	filter(grepl("A|B", wells)) %>% 
	mutate(day = mdy("7/29/18"))


day4 <- read_excel("data/Day 4 reads.xlsx", skip = 26) %>% 
	select(-X__2) %>% 
	rename(row = X__1)
write_csv(day4, "data-processed/pilot-day4.csv")
day4df <- read_plate(file = "data-processed/pilot-day4.csv", well_ids_column = "wells") %>% 
	filter(grepl("A|B", wells)) %>% 
	mutate(day = mdy("7/30/18"))

day5 <- read_excel("data/Day 5 reads.xlsx", skip = 26) %>% 
	select(-X__2) %>% 
	rename(row = X__1)

write_csv(day5, "data-processed/pilot-day5.csv")
day5df <- read_plate(file = "data-processed/pilot-day5.csv", well_ids_column = "wells") %>% 
	filter(grepl("A|B", wells)) %>% 
	mutate(day = mdy("7/31/18"))



all_days <- bind_rows(day1df, day3df, day4df, day5df) %>% 
	rename(rfu = row)

str(all_days)
str(plate_pilot)
plate_all <- left_join(all_days, plate_pilot, by = "wells") %>% 
	mutate(concentration = str_replace(concentration, "is to", "in")) %>% 
	mutate(concentration = as.factor(concentration))

plate_all$concentration <- ordered(plate_all$concentration, levels = c("1 in 100", "1 in 10", "1 in 5", "Full"))

str(plate_all)

plate_all %>% 
	ggplot(aes(x = day, y = rfu, color = concentration, group = wells)) + geom_point(size = 3) +
	ylab("RFU") + xlab("Date") + geom_line()
ggsave("figures/pilot-RFU-over-time.pdf", width = 8, height = 6)

plate_all %>% 
	ggplot(aes(x = day, y = rfu, color = concentration, group = wells)) + geom_point(size = 2) +
	ylab("RFU") + xlab("Date") +
	facet_wrap( ~ concentration, scales = "free") + geom_line()
ggsave("figures/pilot-RFU-over-time-facet.pdf", width = 8, height = 6)

