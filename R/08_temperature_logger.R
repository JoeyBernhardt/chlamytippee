### plot temperature data


library(tidyverse)
library(janitor)
library(lubridate)
library(cowplot)


inc5 <- read_csv("data/temperature-loggers/logger31-05C-2018-09-18.csv", skip = 1) %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(time)) %>% 
	rename(temperature = number_1_o_c) %>% 
	mutate(day = day(date_time)) %>% 
	mutate(month = month(date_time)) %>% 
	unite(month, day, col = "month_day") %>% 
	mutate(date = date(date_time)) %>% 
	mutate(hour = hour(date_time))


inc5 %>%
	mutate(daytime = NA) %>% 
	mutate(daytime = ifelse(hour > 9 & hour < 15.5, "night", "day")) %>% 
	filter(date > ymd("2018-09-14")) %>% 
	ggplot(aes(x = date_time, y = temperature, color = daytime)) + geom_point()


inc25 <- read_csv("data/temperature-loggers/logger30-25C-2018-09-13.csv", skip = 1) %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(time)) %>% 
	rename(temperature = number_1_o_c) %>% 
	mutate(day = day(date_time)) %>% 
	mutate(month = month(date_time)) %>% 
	unite(month, day, col = "month_day") %>% 
	mutate(date = date(date_time)) %>% 
	mutate(hour = hour(date_time))

inc25 %>% 
	mutate(daytime = NA) %>% 
	mutate(daytime = ifelse(hour > 9 & hour < 15.5, "night", "day")) %>% 
	filter(date > ymd("2018-09-09")) %>% 
	ggplot(aes(x = date_time, y = temperature, color = daytime)) + geom_point()


inc20 <- read_csv("data/temperature-loggers/logger36-20C-2018-09-13.csv", skip = 1) %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(time)) %>% 
	rename(temperature = number_1_o_c) %>% 
	mutate(day = day(date_time)) %>% 
	mutate(month = month(date_time)) %>% 
	unite(month, day, col = "month_day") %>% 
	mutate(date = date(date_time)) %>% 
	mutate(hour = hour(date_time))

inc20 %>% 
	mutate(daytime = NA) %>% 
	mutate(daytime = ifelse(hour > 9 & hour < 15.5, "night", "day")) %>% 
	filter(date > ymd("2018-09-06")) %>% 
	filter(temperature < 22) %>% 
	# mutate(date_time = as.date(date_time)) %>% 
	ggplot(aes(x = hour, y = temperature, color = daytime)) + geom_point() +
	facet_wrap( ~ month_day)
	scale_x_date(date_minor_breaks = "1 hour", format = "%d - %H")

