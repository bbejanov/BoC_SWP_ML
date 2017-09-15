#!/usr/bin/R --no-save

library(plyr)
library(ggplot2)

data <- read.csv("data/swp.csv", stringsAsFactors=FALSE)

data <- mutate(data, year=lapply(date, function(x) strsplit(x, " ")[[1]][[2]])) 
data$year <- as.numeric(data$year)

data <- mutate(data, month=lapply(date, function(x) strsplit(x, " ")[[1]][[1]])) 
data$month <- factor(data$month, levels=month.name)

ddply(data, c("year", "month"), summarize, count=length(year))


##
## Part 2: 

library(tm)
library(wordcloud)

make_corp <- function(x) {
    corp <- SimpleCorpus(VectorSource(x))
    corp <- tm_map(corp, tolower)
    corp <- tm_map(corp, removePunctuation)
    corp <- tm_map(corp, function(x) removeWords(x, stopwords()))
    return(corp)
}
corp <- make_corp(data$abstract)
wordcloud(corp[ data$year==2010 ], max.words=50)

for ( y in 2010:2017 ) {
    pdf_name = file.path("plots", "wordcloud", 
                         paste0("wordcloud_", y, ".pdf"))
    pdf(file=pdf_name, width=6, height=6)
    wordcloud(corp[data$year==y], max.words=75, random.order=FALSE)
    text(x=0.0, y=0.95, labels=y, pos=4, cex=3.5, col="brown")
    graphics.off()
}



##
# compare two years

byYear = list()
for (y in unique(data$year)) {
    byYear <- c(byYear, paste(data$abstract[ data$year==y ], collapse=" "))
}

corp.byYear <- SimpleCorpus(VectorSource(byYear))
corp.byYear <- tm_map(corp.byYear, tolower)
corp.byYear <- tm_map(corp.byYear, removePunctuation)
corp.byYear <- tm_map(corp.byYear, function(x) removeWords(x, stopwords()))

tdm <- TermDocumentMatrix(corp.byYear)
tdm <- as.matrix(tdm)  
colnames(tdm) <- c(as.character(2010:2017))
comparison.cloud(tdm[,3:4], max.words=50, random.order=FALSE)

for (i in 1:7) {
    pdf_name = file.path("plots", "wordcloud", 
                         paste0("compare_", colnames(tdm)[i], "_", colnames(tdm)[1+i], ".pdf"))
    pdf(file=pdf_name, width=6, height=6)
    comparison.cloud(tdm[,c(i,i+1)], max.words=75, random.order=FALSE)
    graphics.off()
}



wordcloud(corp.byYear, max.words=100, random.order=FALSE)


