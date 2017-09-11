
library(plyr)
library(ggplot2)
library(igraph)

data <- read.csv("data/swp.csv", stringsAsFactors=FALSE)

all.cases <- as.character(2010:2017)
for (x in 2016:2010) {
   all.cases <- c(all.cases, paste( as.character(seq(x,2017)),collapse="|"))
}

for (year in all.cases) {

authors <- ddply(data[grepl(year, data$date),], ~ id, function(row) data.frame(
                     author=strsplit(row$authors, ",")[[1]]
            ))

###################################
###   data consistency check    ###
#
# make sure each author is either always internal or always external
# 

#tmp <- ddply(authors, ~ author, summarize, len=length(unique(is.boc)))
#if (any(tmp$len != 1)) stop("authors data is inconsistent")


###################
###   some summary statistics  
#
# number of publications

adata <- ddply(authors, ~author, summarize, npub=length(author))

# number of coauthors

#autoinc <- function() { count <- 0; function() {count <<- count + 1; count} }
#autoid <- autoinc()

anodes <- mutate(adata, aid=rownames(adata), weight=npub)[, c("aid", "author", "weight")]

all.pairs <- function(vals) {
    if (length(vals) < 2) 
        return( data.frame() )
    tmp <- combn(vals, 2)
    return(data.frame(a1=tmp[1,], a2=tmp[2,]))
}

author.id <- function(df) {
    join(df["author"], anodes, by="author", type="left")[["aid"]]
}

aedges <- ddply(authors, ~id, function(x) {
                df <- all.pairs(author.id(x))
                if (nrow(df) > 0) {
                    df <- mutate(df, weight=rep(1.0, nrow(df)))
                    return(df[c("a1", "a2", "weight")])
                } else return(data.frame())
            })[c("a1", "a2", "weight")]

net <- graph_from_data_frame(d=aedges, vertices=anodes, directed=FALSE)
net <- simplify(net, edge.attr.comb=list(weight="sum", "ignore"))

min_npub <- min(anodes$weight)
max_npub <- max(anodes$weight)
step_npub <- 7.0/(max_npub-min_npub)

pdf_fname = file.path("plots", 
        paste0("network_", 
               do.call(paste, c(strsplit(year, "\\|"), collapse="_")), 
               ".pdf"))
cat("Saving to", pdf_fname, "...")
pdf(file=pdf_fname, width=6, height=6)
plot(net, 
     vertex.label.cex=.4,
     vertex.size=3+(V(net)$weight-min_npub)*step_npub,
     vertex.label=V(net)$weight, 
     edge.curved=.2, 
     main=year
)
graphics.off()
cat("done\n")

} 


