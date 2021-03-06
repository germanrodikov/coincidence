x <- c(23, 15, 46, NA)
z <- c(5, 6, NA, 8)

mean(x)
mean(x, na.rm = TRUE)
mean(z, na.rm = TRUE)

sum(z, na.rm = TRUE)


d <- data.frame (rost = x, ves = z)
d

d[4,1]
d[,2]

d[4,2]

d$rost
d[,1]
d

my_list <- list(a=7, b=10:20, table=d)
my_list$a
my_list$b
my_list$table

my_list[[2]]

install.packages(c("dplyr", "ggplot2", "GGally", "psych"))
install.packages("devtools")
library(devtools)
install_github("bdemeshev/sophisthse")

library(dplyr)
library(ggplot2)
library(GGally)
library(psych)

help("predict")
help("??describe")

help(lm)
