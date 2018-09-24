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
ggsave("figures/temperature_logger5.pdf", width = 5, height = 4)


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
	ggplot(aes(x = date_time, y = temperature, color = daytime)) + geom_point()
ggsave("figures/temperature_logger20.pdf", width = 5, height = 4)
	

inc10 <- read_csv("data/temperature-loggers/logger39-10C-2018-09-18.csv", skip = 1) %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(time)) %>% 
	rename(temperature = number_1_o_c) %>% 
	mutate(day = day(date_time)) %>% 
	mutate(month = month(date_time)) %>% 
	unite(month, day, col = "month_day") %>% 
	mutate(date = date(date_time)) %>% 
	mutate(hour = hour(date_time))

inc10 %>% 
	mutate(daytime = NA) %>% 
	mutate(daytime = ifelse(hour > 9 & hour < 15.5, "night", "day")) %>% 
	filter(date > ymd("2018-09-12")) %>% 
	filter(temperature < 13) %>% 
	# mutate(date_time = as.date(date_time)) %>% 
	ggplot(aes(x = date_time, y = temperature)) + geom_point() +geom_line()
ggsave("figures/temperature_logger10.pdf", width = 5, height = 4)


inc35 <- read_csv("data/temperature-loggers/logger33-35C-2018-09-18.csv", skip = 1) %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(time)) %>% 
	rename(temperature = number_1_o_c) %>% 
	mutate(day = day(date_time)) %>% 
	mutate(month = month(date_time)) %>% 
	unite(month, day, col = "month_day") %>% 
	mutate(date = date(date_time)) %>% 
	mutate(hour = hour(date_time))

inc35 %>% 
	mutate(daytime = NA) %>% 
	mutate(daytime = ifelse(hour > 9 & hour < 15.5, "night", "day")) %>% 
	filter(date > ymd("2018-09-12")) %>% 
	filter(temperature > 33) %>% 
	# mutate(date_time = as.date(date_time)) %>% 
	ggplot(aes(x = date_time, y = temperature)) + geom_point() +geom_line()
ggsave("figures/temperature_logger35.pdf", width = 10, height = 4)


inc8 <- read_csv("data/temperature-loggers/logger31-08C-2018-09-24.csv", skip = 1) %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(time)) %>% 
	rename(temperature = number_1_o_c) %>% 
	mutate(day = day(date_time)) %>% 
	mutate(month = month(date_time)) %>% 
	unite(month, day, col = "month_day") %>% 
	mutate(date = date(date_time)) %>% 
	mutate(hour = hour(date_time))


inc8 %>%
	mutate(daytime = NA) %>% 
	mutate(daytime = ifelse(hour > 7 & hour < 14.5, "night", "day")) %>% 
	filter(date > ymd("2018-09-18"), temperature < 15) %>% 
	ggplot(aes(x = date_time, y = temperature)) + geom_point() + geom_line()
ggsave("figures/temperature_logger8.pdf", width = 8, height = 4)


inc10 <- read_csv("data/temperature-loggers/logger39-10C-2018-09-24.csv", skip = 1) %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(time)) %>% 
	rename(temperature = number_1_o_c) %>% 
	mutate(day = day(date_time)) %>% 
	mutate(month = month(date_time)) %>% 
	unite(month, day, col = "month_day") %>% 
	mutate(date = date(date_time)) %>% 
	mutate(hour = hour(date_time))


inc10 %>%
	mutate(daytime = NA) %>% 
	mutate(daytime = ifelse(hour > 7 & hour < 14.5, "night", "day")) %>% 
	filter(date > ymd("2018-09-11"), temperature < 12) %>% 
	ggplot(aes(x = date_time, y = temperature)) + geom_point() + geom_line()
ggsave("figures/temperature_logger10-2018-09-24.pdf", width = 8, height = 4)


inc35 <- read_csv("data/temperature-loggers/logger33-35C-2018-09-24.csv", skip = 1) %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(time)) %>% 
	rename(temperature = number_1_o_c) %>% 
	mutate(day = day(date_time)) %>% 
	mutate(month = month(date_time)) %>% 
	unite(month, day, col = "month_day") %>% 
	mutate(date = date(date_time)) %>% 
	mutate(hour = hour(date_time))


inc35 %>%
	mutate(daytime = NA) %>% 
	mutate(daytime = ifelse(hour > 7 & hour < 14.5, "night", "day")) %>% 
	filter(date > ymd("2018-09-11"), temperature > 33) %>% 
	ggplot(aes(x = date_time, y = temperature)) + geom_point() + geom_line()
ggsave("figures/temperature_logger35-2018-09-24.pdf", width = 8, height = 4)




