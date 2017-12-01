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

    foo <- xpathApply(doc, "//div[@class='topic taxonomy']/a", xmlValue)
    if (length(foo) < 1) stop("unable to parse topics in ", fname)
    ret[["topics"]] <- do.call(paste, c(sep="|", foo)) 

    foo <- xpathApply(doc, "//div[@class='jel taxonomy']/a", xmlValue)
    if (length(foo) < 1) {
        ret[["jel"]] <- ""
    } else {
        ret[["jel"]] <- do.call(paste, c(sep="|", foo)) 
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

fuzzy_author_pattern <- function(authors) {
    fap <- gsub("\\s+", " (?:\\\\S* ){0,2}", authors)
    return (fap)
}

markers_pattern <- function(authors) {
    fap <- fuzzy_author_pattern(authors)
    pat1 <- paste(c(fap,""), collapse=",?(\\d?)\\s?(?:,|and)*\\s?")
    pat <- paste0("^.*", pat1, ".*$")
    return(pat)
}

parse_affil <- function(row, kind, authors, pdftext) {
    
    BoCE <- "Bank of Canada"
    BoCF <- "Banque du Canada"

    # Typos in names
    if (kind=="swp") 
        if(row$id=="2016-60") 
            authors[[1]] <- "Teodoara Paligorova"
        else if(row$id=="2017-6") 
            authors[[2]] <- "Josef Schoth"
        else if(row$id=="2010-13") 
            authors[[2]] <- "Yinan Zhang"
        else if(row$id=="2015-8")
            authors[[1]] <- "Jose Garralda"
        else if(row$id=="2015-42")
            authors[[3]] <- "Jeffrey Harris"
        else if (row$id == "2017-43") 
            authors[c(2,3)] <- c("Geoffrey Dunbar", "Q. Rallye Shen")
        else if (row$id == "2010-34")
            authors[[2]] <- "Fabio Rumler"
        else if (any(row$id == c("2010-9", "2013-8"))) 
            authors[[1]] <- "Carlos de Resende"
        else if (row$id == "2011-1")
            authors[[1]] <- "Hector Perez-Saiz"
        else if (row$id == "2012-16")
            authors[[1]] <- "Don Coletti"
        else if (row$id == "2012-22")
            authors[[1]] <- "Evren Damar"
        else if (row$id == "2012-23")
            authors[[2]] <- "Tom Carter"
        else if (row$id=="2015-46") # authors in different order
            authors <- authors[c(2,4,1,3)]


    # Special cases
    if (kind=="swp" && any(authors == "Warren E. Weber")) {
        row$language <- "English"
        affils <- paste("BoC", "Visiting Scholar at Currency Department")
        if (length(authors) > 1) { 
            affils <- c("BoC Currency Department", 
                        "BoC Funds Management and Banking Department", 
                        "BoC Visiting Scholar at Currency Department")
        }
    } else if (kind=="swp" && row$id == "2013-36") {
        row$language <- "English"
        affils <- c("BoC Currency Department",
                    "BoC Financial Markets Department",
                    "Department of Economics, Lakehead University") 
    } else if (kind=="swp" && row$id == "2015-21") {
        row$language <- "English"
        affils <- c("BoC Executive and Legal Services",
                    "BoC International Economic Analysis Department",
                    "BoC Canadian Economic Analysis Department") 
    } else if (kind=="swp" && row$id == "2016-33") {
        row$language <- "English"
        affils <- c("BoC Funds Management and Banking Department",
                    "University of Bern") 
    } else if (kind=="swp" && row$id == "2016-2") {
        row$language <- "English"
        affils <- c("McMaster University",
                    "BoC Canadian Economic Analysis Department") 
    } else if (kind=="swp" && row$id == "2012-17") {
        row$language <- "English"
        affils <- paste("BoC", "Funds Management and Banking Department")
    } else if (kind=="sdp" && row$id == "2014-6") {
        row$language <- "English"
        affils <- paste("BoC", "Governor")
    } else if(kind=="swp" && row$id=="2016-30") {
        row$language <- "English"
        affils <- c("European Central Bank", 
                    "BoC Economic and Financial Research")
    } else if (kind=="swp" && row$id=="2016-32") {
        row$language <- "English"
        affils <- c("BoC International Economic Analysis Department",
                    "Departemnt of Economics, University of Notre Dame and NBER")
    } else if (kind=="swp" && row$id=="2016-36") {
        row$language <- "English"
        affils <- c("Bank of Japan",
                    "BoC Financial Stability Department")
    } else if (kind=="swp" && row$id=="2017-47") {
        row$language <- "English"
        affils <- c("BoC Currency Department", 
                    "BoC Currency Department", 
                    "ozshy@ozshy.com")
    } else if (kind=="swp" && row$id=="2013-37") {
        row$language <- "English"
        affils <- c("BoC Financial Markets Department", 
                    "BoC Financial Markets Department")
    } else if (kind=="swp" && row$id=="2015-38") {  
        row$language <- "English"
        affils <- rep("BoC International Economic Analysis Department", 
                      length(authors))
    } else {
        # General case
        all_text <- iconv(paste(pdftext, collapse=","),
                          to="ASCII//TRANSLIT")
        mp <- iconv(markers_pattern(authors), to="ASCII//TRANSLIT")
        stopifnot(grepl(mp, all_text))
    
        repl <- paste(" \\",seq_along(authors), sep="", collapse="|")
        markers <- sub(mp, repl, all_text)
        markers <- strsplit(markers, "\\|")[[1]]
        markers <- trimws(markers)
    
        if (all(nchar(markers))==0) {
            # all authors are from the same Bank dept
            dept_pat <- gsub("\\(\\\\d\\?\\)", "", mp)
            dept_pat <- sub("\\.\\*\\$", "", dept_pat)
            dept_pat <- paste0(dept_pat, ",(.*?),\\s*", BoCE, ".*$")
            if (grepl(dept_pat, all_text)) {
                row$language <- "English"
            } else {
                dept_pat <- sub(BoCE, BoCF, dept_pat)
                if (grepl(dept_pat, all_text)) {
                    row$language <- "Français"
                } else {
                    stop("dept_pat does not match")
                }
            }
            dept <- paste("BoC", trimws(sub(dept_pat, "\\1", all_text))) 
            affils <- rep(dept, length(authors))
        } else if (all(nchar(markers))==1) {
            row$language <- "English"
            # we reuse the mp pattern, but we need to 
            # make the marker groups non-capturing and 
            # strip the .*$ at the end
            amp <- sub("\\.\\*\\$", "", gsub("\\(\\\\d\\?\\)", "(?:\\\\d?)", mp))
            affils <- rep(NA, length(authors))
            um <- unique(markers)
            num <- length(um)
            aff_p <- paste0(",", um, "(.*?)", collapse="")
            ap <- paste0(amp, ".*", aff_p, ".*?$")
            stopifnot(grepl(ap, all_text))
            affils <- sub(ap, paste0(" \\", um, collapse="|"), all_text)
            affils <- strsplit(affils, "\\|")[[1]]
            affils <- sub("Corresponding author:?", "", affils, ignore.case=TRUE)
            affils <- strsplit(affils, ",")
            affils <- lapply(affils, trimws)
            affils <- lapply(affils, function(a) a[nchar(a)>0])
            laffils <- sapply(affils, length)
            if(all(laffils[-num]==1) && laffils[[num]] > 1 
               && affils[[num]][[2]] == BoCE) {
                row$language <- "English"
                affils <- lapply(affils, function(a) paste("BoC", a[[1]]))
            } else if (all(laffils[-num]==1) && laffils[[num]] > 1 
                       && affils[[num]][[2]] == BoCF) {
                row$language <- "Français"
                affils <- lapply(affils, function(a) paste("BoC", a[[1]]))
            } else {
                affils <- lapply(affils, function(a) {
                    if (length(a) < 2) return(a[[1]])
                    if (a[[2]]==BoCE) {
                        return(paste("BoC", a[[1]]))
                    } else if (a[[2]]==BoCF) {
                        row$language <- "Français"
                        return(paste("BoC", a[[1]]))
                    } else if (grepl(paste("university","universite",
                                           "universidad",
                                           "bank","banque","fund","board",
                                           "bureau", "statistics canada", 
                                           "\\w+ school of economics",
                                           "\\w+ school of management",
                                           "\\w+ business school",
                                           sep="|"), 
                                     a[[1]], ignore.case=TRUE)) {
                        return(a[[1]]) 
                    } else {
                        return(paste(a[1:2], collapse=", "))
                    }})
            }
            affils <- affils[as.numeric(markers)]
        } else {
            stop("Cannot handle case of some markers but not all")
        }
    }
    
    cat("  ", row$language, "\n")
    row$affils <- paste(affils, collapse="|")
    for(i in seq_along(authors))
        cat("    ", authors[[i]], "->", affils[[i]], "\n")

    return(row)

}

 
get_pdftext <- function(row, kind) {
    # download the pdf file (if not already downloaded)   
    pdffile <- file.path("data", "pdf", kind, basename(row$pdf))
    if (file.exists(pdffile) == FALSE) 
        download.file(row$pdf, pdffile, quiet=TRUE)
    stopifnot(file.exists(pdffile))
    #extract the text from page 2 - the title page with authors' info
    pf <- pipe(paste("pdftotext -raw -f 2 -l 2 2>/dev/null", pdffile, "-"))
    pdftext <- readLines(pf, warn=FALSE)
    close(pf)
    return(pdftext)
}

get_affil <- function(row, kind) {
    cat(row$id, ":\n")
    pdftext <- get_pdftext(row, kind)
    # how many authors?
    authors <- strsplit(row$authors, "\\|")[[1]]

    row <- parse_affil(row, kind, authors, pdftext)
    return(row)

}

if(FALSE) {
    kind <- "swp"
    for(fn in list.files(file.path("data", kind))) 
        foo <- get_affil(parse_row(file.path("data", kind, fn)),kind)
}


#if(FALSE)
# for (kind in c("swp", "san", "sdp")) {
for (kind in c("swp")) {
    cat("Parsing", kind, "...")
    if (dir.exists(file.path("data", "pdf", kind)) == FALSE)
        dir.create(file.path("data", "pdf", kind), recursive=TRUE)
    data <- data.frame()
        dirname <- file.path("data", kind)
        for( fn in list.files(dirname) ) {
            row <- parse_row(file.path(dirname, fn))
            row <- get_affil(row, kind)
            data <- rbind(data, as.data.frame(row, stringsAsFactors=FALSE))
        }
    cat("saving", nrow(data), "records\n")
    write.csv(data, file=file.path("data", paste(kind, "csv", sep=".")), 
              row.names=FALSE)
}

