#!/usr/bin/R --no-save

library(XML)

parse_row <- function(fname) {
    doc <- htmlParse(fname)

    ret <- list()

    foo <- xpathApply(doc, "//div[@class='post-type']", xmlValue)
    if (length(foo) != 1) stop("unable to parse title in ", fname)
    ret[["id"]] <- regmatches(foo[[1]], regexpr("\\d{4}-\\d+", foo[[1]]))

    foo <- xpathApply(doc, "//h1[@class='post-heading']", xmlValue)
    if (length(foo) != 1) stop("unable to parse title in ", fname)
    ret[["title"]] <- foo[[1]] 

    foo <- xpathApply(doc, "//div[@class='post-date']", xmlValue)
    if (length(foo) != 1) stop("unable to parse date in ", fname)
    ret[["date"]] <- foo[[1]] 

    foo <- xpathApply(doc, "//div[@class='post-authors']/a", xmlValue)
    if (length(foo) < 1) stop("unable to parse authors in ", fname)
    ret[["authors"]] <- do.call(paste, c(sep=",", foo)) 

    foo <- xpathApply(doc, "//div[@class='post-authors']/a", 
                      xmlGetAttr, "title", default=FALSE, 
                      converter=function(...)TRUE)
    if (length(foo) < 1) stop("unable to parse authors titles in ", fname)
    ret[["authorsInternal"]] <- paste(foo, collapse=",")

    foo <- xpathApply(doc, "//div[@class='topic taxonomy']/a", xmlValue)
    if (length(foo) < 1) stop("unable to parse topics in ", fname)
    ret[["topics"]] <- do.call(paste, c(sep=",", foo)) 

    foo <- xpathApply(doc, "//div[@class='jel taxonomy']/a", xmlValue)
    if (length(foo) < 1) {
        ret[["jel"]] <- ""
    } else {
        ret[["jel"]] <- do.call(paste, c(sep=",", foo)) 
    }

    foo <- xpathApply(doc, "//span[@class='post-content']", xmlValue)
    if (length(foo) != 1) stop("unable to parse abstract in ", fname)
    ret[["abstract"]] <- trimws(strsplit(trimws(foo[[1]]), "\\n|\\r")[[1]][[1]]) 

    return( ret )
}

for (kind in c("swp", "san", "sdp")) {
    cat("Parsing", kind, "...")
    data <- data.frame()
    #for (year in list.files("data") ) {
        dirname <- file.path("data", kind)
        for( fn in list.files(dirname) ) {
            row <- parse_row(file.path(dirname, fn))
            data <- rbind(data, as.data.frame(row, stringsAsFactors=FALSE))
        }
    #}
    cat("saving", nrow(data), "records\n")
    write.csv(data, file=file.path("data", paste(kind, "csv", sep=".")), row.names=FALSE)
}

