library(MASS)
library(dplyr)

#============================================================================
year <- "2013|2014|2015|2016|2017"
pngfile <- NULL # "plots/net_2013_2017.png"
source("authors.r")

starting_points <- function(net, affil.levels=levels(authors$affil)) {
    centre <- function(code) {
        if (code == 1 || code == 8) c(0,0) else
            c( cos(2*pi*code/6), sin(2*pi*code/6) ) 
    }
    coords <- lapply(V(net)$name, function(c){
        if(nchar(c) == 3) {
            dep_code <- match(c, affil.levels)
            mu <- centre(dep_code)
            sig <- diag(c(0,0))
        } else {
            dep <- strsplit(c, ",")[[1]][[2]]
            dep_code <- match(dep, affil.levels)
            mu <- centre(dep_code)
            sig <- if(dep_code==8) diag(c(1.5, 1.5)) else diag(c(0.3, 0.3))
        }
        return( mvrnorm(mu=mu, Sigma=sig) )
    })
    as.mat <- matrix(unlist(coords), ncol = 2, byrow = TRUE)
    return(as.mat)
}


min_npub <- min(V(net)$weight)
max_npub <- max(V(net)$weight)
step_npub <- min(1.0, 7.0/(max_npub-min_npub))

if(!is.null(pngfile))
    png(filename=pngfile,
        width=600, height=600
    )

plot(net,
     # main="2010",
     vertex.label="",
     vertex.shape=ifelse(V(net)$type==0, "none", "circle"),
     vertex.size=(3+(V(net)$weight-min_npub)*step_npub),
     vertex.color=ifelse(V(net)$type>0, aff_colors[V(net)$type], "white"),
     edge.curved=.2,
     edge.width=pmax(3,E(net)$weight)*E(net)$type/2,
     layout=layout_(net, with_graphopt(charge=0.02, 
                                       spring.constant=10,
                                       # spring.length=0.5,
                                       niter=5000, 
                                       start=starting_points(net)
                                       
     ))
)

depts_present <- sort(unique(V(net)$type))[-1]
depts_names <- sub("OTH", "Other Dept.", sub("EXT", "External", names(aff_colors[depts_present])))
legend("bottomleft", depts_names, fill=aff_colors[depts_present], 
       border=aff_colors[depts_present], bty="n")

if(!is.null(pngfile))
    graphics.off()

#============================================================================
