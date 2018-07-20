library(cowplot)
library(tidyverse)



### Mriduls' double exponential model
library(ggplot2)
B1 <- 1
B2 <- 0.18
D0 <- 1
D1 <- 0.128
D2 <- 0.32

B1 <- data %>% 
	filter(Species == "CS", term == "b1") %>% 
	select(estimate)
B2 <- data %>% 
	filter(Species == "CS", term == "b2") %>% 
	select(estimate)
D0 <- data %>% 
	filter(Species == "CS", term == "d0") %>% 
	select(estimate)
D1 <- data %>% 
	filter(Species == "CS", term == "d1") %>% 
	select(estimate)
D2 <- data %>% 
	filter(Species == "CS", term == "d2") %>% 
	select(estimate)


B1 <- 0.08
B2 <- 0.09
D0 <- 0
D1 <- 5.952219e-06
D2 <- 0.39

x <- seq(0, 40, by =0.1)

mod_pred <- function(x) {
	y <- (B1[[1]]*exp(B2[[1]]*x) - (D0[[1]] + (D1[[1]] *exp(D2[[1]]*x))))
}

mod_predictions<- data.frame(x, growth = sapply(x, mod_pred))
p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
p + geom_line(aes(x = x, y = growth), data = mod_predictions) +
	ylim(0, 1) + xlab("Temperature (°C)") + ylab("Growth rate")

get_topt <- function(df){
	grfunc<-function(x){
		-mod_pred(x)
	}
	?optim
	optinfo<-optim(c(20,grfunc))
	opt <-c(optinfo$par[[1]])
	maxgrowth <- c(-optinfo$value)
	results <- data.frame(topt = opt, rmax = maxgrowth)
	return(results)
}

get_topt

data <- read_delim("data/all_species_IDE.txt", delim = ",")
data %>% 
	filter(Species == "AC", term == "b1") %>% 
	select(estimate)
