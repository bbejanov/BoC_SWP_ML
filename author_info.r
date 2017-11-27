
swp <- read.csv("data/swp.csv", header=TRUE, stringsAsFactors=FALSE)

parse_authors_affiliation <- function(auth, lines) {
    if (nauth == 1) {
        
    }

}

for (ind in seq_len(nrow(swp))) {
    pdffile <- swp[ind,"pdf"]
    txtfile <- gsub("pdf", "txt", sub(".*/", "data/pdf/swp/", pdffile))
    txtlines <- readLines(txtfile, warn=FALSE)
    auth <- strsplit(swp[ind, "authors"], ",")[[1]]
    nauth <- length(auth)

    
 
}



