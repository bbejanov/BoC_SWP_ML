library(MASS)
library(dplyr)

#============================================================================
year <- paste(seq.int(2010,2017), collapse="|")
source("authors.r")

stats <- authors %>% 
    mutate(year=sub("-\\d*$", "", id)) %>% 
    group_by(year, affil) %>% 
    summarize(num_authors=n_distinct(author)) %>% 
    arrange(year, affil)
write.csv(stats, file="by_year_affil.csv", row.names=FALSE)

stats <- authors %>% 
    mutate(year=sub("-\\d*$", "", id)) %>% 
    group_by(year, affil) %>% 
    group_by(year, author, affil) %>%
    summarize(num_papers=n()) %>% 
    ungroup() %>%
    group_by(year, affil, num_papers) %>%
    summarize(num_authors=n())
write.csv(stats, file="by_year_affil_npublications.csv", row.names=FALSE)

stats <- authors %>% 
    mutate(year=sub("-\\d*$", "", id)) %>% 
    filter(as.numeric(year) >= 2013) %>% 
    group_by(author, affil) %>%
    summarize(num_papers=n()) %>% 
    arrange(-num_papers, affil, author)
write.csv(stats, file="num_papers_2013_2017.csv", row.names=FALSE)



