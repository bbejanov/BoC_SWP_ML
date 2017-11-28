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
    ret[["authors"]] <- do.call(paste, c(sep="\\|", foo)) 

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

    foo <- xpathApply(doc, "//div[contains(@class,'post-formats')]/a", 
                      xmlGetAttr, "href")
    if (length(foo) != 1) stop("unable to parse the pdf link in ", fname)
    ret[["pdf"]] <- trimws(foo[[1]])

    foo <- xpathApply(doc, "//span[@class='post-content']", xmlValue)
    if (length(foo) != 1) stop("unable to parse abstract in ", fname)
    ret[["abstract"]] <- trimws(strsplit(trimws(foo[[1]]), "\\n|\\r")[[1]][[1]]) 

    return( ret )
}

get_affil <- function(row, kind) {
    # download the pdf file (if not already downloaded)   
    pdffile <- file.path("data", "pdf", kind, basename(row$pdf))
    if (file.exists(pdffile) == FALSE) 
        download.file(row$pdf, pdffile, quiet=TRUE)
    stopifnot(file.exists(pdffile))
    cat(basename(pdffile), ":\n")
    #extract the text from page 2 - the title page with authors' info
    pf <- pipe(paste("pdftotext -raw -f 2 -l 2", pdffile, "-"))
    pdftext <- readLines(pf, warn=FALSE)
    close(pf)
    # how many authors?
    authors <- strsplit(row$authors, "\\|")[[1]]
    if(length(authors)==1) {
        all_text <- paste(pdftext, collapse=",")
        pattern <- paste0("^.*", authors[[1]], "\\s*,(.*),\\s*Bank of Canada.*$")
        stopifnot(grepl(pattern, all_text))
        row$affils <- sub(pattern, "\\1", all_text)
        
    cat(authors[[1]], "->", row$affils, "\n")

    } else {
    }

    return(row)
}

if(FALSE) {
    kind <- "swp"
    for(fn in list.files(file.path("data", kind))) 
        foo <- get_affil(parse_row(file.path("data", kind, fn)),kind)
}


if(FALSE)
for (kind in c("swp", "san", "sdp")) {
    cat("Parsing", kind, "...")
    if (dir.exists(file.path("data", "pdf", kind)) == FALSE)
        dir.create(file.path("data", "pdf", kind), recursive=TRUE)
    data <- data.frame()
        dirname <- file.path("data", kind)
        for( fn in list.files(dirname) ) {
            row <- parse_row(file.path(dirname, fn))
            data <- rbind(data, as.data.frame(row, stringsAsFactors=FALSE))
        }
    cat("saving", nrow(data), "records\n")
    write.csv(data, file=file.path("data", paste(kind, "csv", sep=".")), row.names=FALSE)
}

