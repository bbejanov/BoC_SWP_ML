
library(plyr)
library(ggplot2)
library(igraph)

data <- read.csv("data/swp.csv", stringsAsFactors=FALSE)

all.cases <- as.character(2010:2017)
for (x in 2016:2010) {
   all.cases <- c(all.cases, paste( as.character(seq(x,2017)),collapse="|"))
}

for (year in all.cases) {

    fname_suffix <- do.call(paste, c(strsplit(year, "\\|"), collapse="_"))

    # authors in the given year
    authors <- ddply(data[grepl(year, data$date),], ~ id, function(row) data.frame(
                     author=strsplit(row$authors, ",")[[1]]
            ))


    ###################
    ###   some summary statistics  
    #

    # number of authors for each paper
    pdata <- ddply(authors, ~id, summarize, nauth=length(id))
    
    qp <- qplot(nauth, data=pdata, xlab="Number of Authors", ylab="Number of Papers",
                geom="histogram", main=year)
    ggsave(filename=file.path("plots", "authors", 
                               paste0("nauthors_", fname_suffix, ".pdf")),
           plot=qp, width=6, height=6)


    # number of papers for each author
    adata <- ddply(authors, ~author, summarize, npub=length(author))

    qp <- qplot(npub, data=adata, xlab="Number of Papers", ylab="Number of Authors",
                geom="histogram", main=year)
    ggsave(filename=file.path("plots", "authors", 
                       paste0("npapers_", fname_suffix, ".pdf")), 
        plot=qp, width=6, height=6)

    #######################
    ### co-author graph
    #

    # nodes of graph are authors
    anodes <- mutate(adata, aid=rownames(adata), weight=npub)
    anodes <- anodes[, c("aid", "author", "weight")]

    all.pairs <- function(vals) {
        if (length(vals) < 2) 
            return( data.frame() )
        tmp <- combn(vals, 2)
        return(data.frame(a1=tmp[1,], a2=tmp[2,]))
    }

    author.id <- function(df) {
        join(df["author"], anodes, by="author", type="left")[["aid"]]
    }

    # edge exists if two authors have a publication together
    # weight of edge is the number of publications
    aedges <- ddply(authors, ~id, function(x) {
                df <- all.pairs(author.id(x))
                if (nrow(df) > 0) {
                    df <- mutate(df, weight=rep(1.0, nrow(df)))
                    return(df[c("a1", "a2", "weight")])
                } else return(data.frame())
            })
    aedges <- aedges[c("a1", "a2", "weight")]

    net <- graph_from_data_frame(d=aedges, vertices=anodes, directed=FALSE)
    net <- simplify(net, edge.attr.comb=list(weight="sum", "ignore"))

    min_npub <- min(anodes$weight)
    max_npub <- max(anodes$weight)
    step_npub <- min(1.0, 7.0/(max_npub-min_npub))

    pdf_fname = file.path("plots", "authors",
                          paste0("network_", fname_suffix, ".pdf"))
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


