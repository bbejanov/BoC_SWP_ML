#!/usr/bin/R --no-save

library(plyr)
library(ggplot2)

data <- read.csv("data/swp.csv", stringsAsFactors=FALSE)

data <- mutate(data, year=lapply(date, function(x) strsplit(x, " ")[[1]][[2]])) 
data$year <- as.numeric(data$year)

data <- mutate(data, month=lapply(date, function(x) strsplit(x, " ")[[1]][[1]])) 
data$month <- factor(data$month, levels=month.name)

ddply(data, c("year", "month"), summarize, count=length(year))


