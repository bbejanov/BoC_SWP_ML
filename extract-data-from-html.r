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
    ret[["authors"]] <- do.call(paste, c(sep="|", foo)) 

    #foo <- xpathApply(doc, "//div[@class='post-authors']/a", 
    #                  xmlGetAttr, "title", default=FALSE, 
    #                  converter=function(...)TRUE)
    #if (length(foo) < 1) stop("unable to parse authors titles in ", fname)
    #ret[["authorsInternal"]] <- paste(foo, collapse=",")

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

parse_one_author <- function(row, kind, author, pdftext) {
    BoCE <- "Bank of Canada"
    BoCF <- "Banque du Canada"
    if (kind=="swp" && author == "Warren E. Weber") {
        row$language <- "English"
        row$affils <- paste("BoC", "Visiting Scholar at Currency Department")
    } else if (kind=="swp" && row$id == "2012-17") {
        row$language <- "English"
        row$affils <- paste("BoC", "Funds Management and Banking Department")
    } else if (kind=="sdp" && row$id == "2014-6") {
        row$language <- "English"
        row$affils <- paste("BoC", "Governor")
    } else {
        all_text <- iconv(paste(pdftext, collapse=","),
                          to="ASCII//TRANSLIT")
        fuzzy_author_pattern <- iconv(gsub("\\s+", " ?\\\\S* ?", author),
                                      to="ASCII//TRANSLIT")
        # note: (.*?) - the ? means shortest match
        pattern <- paste0("^.*", fuzzy_author_pattern, 
                          "\\s*,(.*?),\\s*", BoCE, ".*$")
        if (grepl(pattern, all_text)) {
            row$language <- "English"
        } else {
            pattern <- paste0("^.*", fuzzy_author_pattern,
                              "\\s*,(.*?),\\s*", BoCF, ".*$")
            if (grepl(pattern, all_text)) {
                row$language <- "Français"
            } else {
                stop("pattern does not match")
            }
        }

        row$affils <- paste("BoC", trimws(sub(pattern, "\\1", all_text)))
    }
    return(row)
}

get_affil <- function(row, kind) {
    # download the pdf file (if not already downloaded)   
    pdffile <- file.path("data", "pdf", kind, basename(row$pdf))
    if (file.exists(pdffile) == FALSE) 
        download.file(row$pdf, pdffile, quiet=TRUE)
    stopifnot(file.exists(pdffile))
    cat(basename(pdffile), ":\n")
    #extract the text from page 2 - the title page with authors' info
    pf <- pipe(paste("pdftotext -raw -f 2 -l 2 2>/dev/null", pdffile, "-"))
    pdftext <- readLines(pf, warn=FALSE)
    close(pf)
    # how many authors?
    authors <- strsplit(row$authors, "\\|")[[1]]
    if(length(authors)==1) {
        row <- parse_one_author(row, kind, authors[[1]], pdftext)
        # cat(row$language, "  ", authors[[1]], "->",  row$affils, "\n")
    } else if (length(authors) == 2) {
        cat("    ", row$id)

        BoCE <- "Bank of Canada"
        BoCF <- "Banque du Canada"

        all_text <- iconv(paste(pdftext, collapse=","),
                          to="ASCII//TRANSLIT")
        fuzzy_author_pattern <- iconv(gsub("\\s+", " ?\\\\S* ?", authors),
                                      to="ASCII//TRANSLIT")
        # note: (.*?) - the ? means shortest match
        markers_pattern <- paste0("^.*", 
                          fuzzy_author_pattern[[1]],
                          "(\\d?),? ?and ?,?",
                          fuzzy_author_pattern[[2]],
                          "(\\d?).*$")
        stopifnot(grepl(markers_pattern, all_text))
        markers <- strsplit(sub(markers_pattern, "\\1 | \\2", all_text), "\\|")[[1]]
        markers <- lapply(markers, trimws)
        
        if (all(nchar(markers)==0)) {
            # two authors from the Bank from the same department
            dept_pattern <- paste0("^.*", 
                          fuzzy_author_pattern[[1]],
                          "\\d? and ",
                          fuzzy_author_pattern[[2]],
                          "\\d?\\s*,(.*?),\\s*", BoCE, ".*$")
            if (grepl(dept_pattern, all_text)) {
                row$language <- "English"
            } else {
                dept_pattern <- sub(BoCE, BoCF, dept_pattern)
                if (grepl(pattern, all_text)) {
                    row$language <- "Français"
                } else {
                    stop("pattern does not match")
                }
            }

            dept <- paste("BoC", trimws(sub(dept_pattern, "\\1", all_text)))
            affils <- c(dept, dept)
        } else {
            stop("2")
        }

        cat("\t", row$language, "\n")
        row$affils <- paste(affils, collapse="|")
        for(i in seq_along(authors)) 
            cat("\t", authors[[i]], "->", affils[[i]], "\n")
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

