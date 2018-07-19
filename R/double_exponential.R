library(cowplot)



### Mriduls' double exponential model
library(ggplot2)
B1 <- 1
B2 <- 0.18
D0 <- 1
D1 <- 0.128
D2 <- 0.32
x <- seq(0, 30, by =0.1)
mod_pred <- function(x) {
	y <- (B1*exp(B2*x) - (D0 + (D1*exp(D2*x))))
}

mod_predictions <- data.frame(x, growth = sapply(x, mod_pred))
p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
p + geom_line(aes(x = x, y = growth), data = mod_predictions) +
	ylim(0, 2) + xlab("Temperature (°C)") + ylab("Growth")

get_topt <- function(df){
	grfunc<-function(x){
		-mod_pred(x)
	}
	optinfo<-optim(c(x=df$z[[1]]),grfunc)
	opt <-c(optinfo$par[[1]])
	maxgrowth <- c(-optinfo$value)
	results <- data.frame(topt = opt, rmax = maxgrowth)
	return(results)
}

get_topt
