

library(tidyverse)
library(stringr)

image_counts <- read_csv("data-processed/96-well-cell-counts.csv")
flow_cytometer_counts <- read_csv("data/flow-cytometry-eawag/gate-r3-counts-ct-day2-50percent.csv") %>% 
	mutate(well = str_replace(well, "0", ""))


images <- image_counts %>% 
	filter(day == "day2") %>% 
	filter(light_level == 50)


counts <- left_join(images, flow_cytometer_counts)

x <- c(100, 1000, 10000, 100000)
y <- c(7, 70, 700, 7000)
plot(x, y)
abline(b = 1)



df <- data.frame(x, y) %>% 
	mutate(x13 = x*13)


df %>% 
	ggplot(aes(x = log(x), y = log(y))) + geom_point() +
	geom_abline(slope = 1, intercept = 0)
### the image area is 789 * 583um which is equivalent to 0.459987mm^2, and total well surface area is 32mm^2

counts2 <- counts %>% 
	mutate(cytometer_count = Count*4*10) %>%
	mutate(image_count = cell_count*69.5672/10*10)

counts2 %>% 
	ggplot(aes(x = image_count, y = cytometer_count)) + geom_point() + 
	ylab("Flow cytometer count (cells/ml)") + xlab("Image cell count (cells/ml)") +
	geom_abline(slope = 1, intercept = 0) 
ggsave("figures/FC-vs-imager.pdf", width = 5, height = 4)


counts2 %>% 
	lm(image_count ~ cytometer_count, data = .) %>% summary()
