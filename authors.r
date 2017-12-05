
library(dplyr)
library(ggplot2)
library(igraph)

data <- read.csv("data/swp.csv", stringsAsFactors=FALSE)

all.cases <- as.character(2010:2017)
for (x in 2016:2010) {
   all.cases <- c(all.cases, paste( as.character(seq(x,2017)),collapse="|"))
}

affil.as.factor <- function (affil) {
    .do <- function(val) {
        if (grepl("^BoC ", val)) {
            val <- sub("^BoC ", "", val)
            return(switch(val, 
                "Adviser to the Governor"="OTH",
                "Canadian Economic Analysis Department"="CEA",
                "Currency Department"="CUR",
                "Executive and Legal Services"="OTH",
                "Financial Markets Department"="FMD",
                "Financial Stability Department"="FSD",
                "Funds Management and Banking Department"="FBD",
                "International Economic Analysis Department"="INT",
                "International Economic Analysis"="INT",
                "Visiting Scholar at Currency Department"="CUR",
                "Governor of Bank of Canada"="OTH",
                "Economic and Financial Research"="OTH",
                "Communications Department"="OTH",
                stop("new department", val)))
        } else {
            return ("EXT")
        }
    }
    foo <- vapply(affil, .do, "", USE.NAMES=FALSE)
    foo <- factor(foo, 
                  levels=c("OTH", "CEA", "INT", "FSD", "FMD", 
                           "FBD", "CUR", "EXT"),
                  ordered=TRUE)
    return (foo)
}

aff_colors <- c(OTH="black", 
                CEA="red", 
                INT="blue", 
                FSD="green", 
                FMD="cyan", 
                FBD="orange", 
                CUR="yellow", 
                EXT="grey80")

plot_net <- function(net, ...) {
    min_npub <- min(V(net)$weight)
    max_npub <- max(V(net)$weight)
    step_npub <- min(1.0, 7.0/(max_npub-min_npub))

    plot(net,
         vertex.label.cex=.8,
         vertex.label=sapply(V(net)$weight, function(x) ifelse(x>1,x,"")),
         vertex.label.color="white",
         vertex.shape=ifelse(V(net)$type==0, "none", "circle"),
         vertex.size=(6+(V(net)$weight-min_npub)*step_npub),
         vertex.color=ifelse(V(net)$type>0, aff_colors[V(net)$type], "white"),
         edge.curved=.2,
         edge.width=E(net)$weight*3*E(net)$type,
         edge.label=sapply(E(net)$weight, function(x) ifelse(x>1, x, "")),
         edge.label.cex=.7,
         main=year, 
         ...)
}

#for (year in all.cases) {
if(!exists("year")) 
    year <- "2014"

if(TRUE) {
    fname_suffix <- do.call(paste, c(strsplit(year, "\\|"), collapse="_"))
    
    # authors in the given year
    authors <- data %>%
        filter(grepl(year, .$date)) %>%
        group_by(id) %>%
        do(data.frame(author=strsplit(.$authors, "\\|")[[1]], 
                      affil=strsplit(.$affils, "\\|")[[1]],
                      stringsAsFactors=FALSE)) %>%
        ungroup() %>%
        mutate(affil=affil.as.factor(.$affil))
        
    
    ###################
    ###   some summary statistics  
    #
if(FALSE) {
#     # number of authors for each paper
#     pdata <- authors %>% group_by(id) %>% summarize(nauth=n())
#     
#     qp <- qplot(nauth, data=pdata, xlab="Number of Authors", ylab="Number of Papers",
#                 geom="histogram", binwidth=0.5, main=year)
#     ggsave(filename=file.path("plots", "authors", 
#                                paste0("nauthors_", fname_suffix, ".pdf")),
#            plot=qp, width=6, height=6)
# 
# 
#     # number of papers for each author
#     adata <- authors %>% group_by(author) %>% summarize(npub=n())
#     qp <- qplot(npub, data=adata, xlab="Number of Papers", ylab="Number of Authors",
#                 geom="histogram", binwidth=0.5, main=year)
#     ggsave(filename=file.path("plots", "authors", 
#                        paste0("npapers_", fname_suffix, ".pdf")), 
#         plot=qp, width=6, height=6)
}
    #######################
    ### co-author graph
    #

    # nodes of graph are authors
    anodes <- authors %>% 
        group_by(author, affil) %>%
        summarize(weight=n()) %>%
        ungroup() %>%
        mutate(name=paste(.$author, .$affil, sep=","),
               type=as.numeric(.$affil)) %>%
        select(name, weight, type)

    ext_nodes <- data.frame(name=levels(authors$affil), weight=0, type=0)
    ext_nodes <- filter(ext_nodes, name!="EXT")
    ext_nodes <- rbind(anodes, ext_nodes)

    all.pairs <- function(vals) {
        if (length(vals) < 2) 
            return( data.frame() )
        tmp <- combn(vals, 2)
        return(data.frame(a1=tmp[1,], a2=tmp[2,], stringsAsFactors=FALSE))
    }

    aedges <- authors %>% 
        group_by(id) %>% 
        do(all.pairs(paste(.$author,.$affil,sep=","))) %>% 
        ungroup() %>% 
        select(a1,a2,id)
       
    ext_edges <- authors %>% 
        filter(affil != "EXT") %>% 
        transmute(a1=affil, a2=paste(author, affil, sep=",")) %>%
        distinct() %>%
        mutate(id="0")

    ext_edges <- rbind(aedges, ext_edges)

    net <- graph_from_data_frame(ext_edges, vertices=ext_nodes, directed=FALSE)
    E(net)$weight <- 1
    net <- simplify(net, edge.attr.comb=list(weight="sum", "ignore"))
    E(net)$type <- 1
    for (dep in levels(authors$affil)) {
        vind <- match(dep, V(net)$name)
        if (is.na(vind) == FALSE) {
            E(net)[from(vind)]$type <- 0
        }
    }



    #V(net)$weight <- unlist(vapply( V(net)$name, 
    #                        function (x) sum(grepl(x, authors$author)),
    #                        0, USE.NAMES=FALSE))

    min_npub <- min(V(net)$weight)
    max_npub <- max(V(net)$weight)
    step_npub <- min(1.0, 7.0/(max_npub-min_npub))

if(FALSE) {
#     pdf_fname = file.path("plots", "authors",
#                           paste0("network_", fname_suffix, ".pdf"))
#     cat("Saving to", pdf_fname, "...")
#     pdf(file=pdf_fname, width=6, height=6)
#     plot(net, 
#          vertex.label.cex=.4,
#          vertex.label=sapply(V(net)$weight, function(x) ifelse(x>1,x,"")),
#          vertex.size=3+(V(net)$weight-min_npub)*step_npub,
#          vertex.col=V(net)$type,
#          edge.curved=.2, 
#          edge.width=E(net)$weight*2,
#          edge.label=sapply(E(net)$weight, function(x) ifelse(x>1, x, "")),
#          edge.label.cex=.7,
#          main=year,
#     )
#     graphics.off()
#     cat("done\n")
}

} 

# plot_net(net, layout=layout_(net, with_graphopt(charge=.05, spring.constant=10, niter=5000)))
# 


