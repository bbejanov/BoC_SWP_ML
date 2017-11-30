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
        else if (row$id=="2015-46") # authors in different order
            authors <- authors[c(2,4,1,3)]


    # Special cases
    if (kind=="swp" && any(authors == "Warren E. Weber")) {
        row$language <- "English"
        affils <- paste("BoC", "Visiting Scholar at Currency Department")
        if (length(authors) > 1) { 
            stop("The case of Warren et al.")
        }
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
    } else if (kind=="swp" && any(row$id==c("2016-32","2016-36"))) {
        row$language <- "English"
        affils <- c("BoC International Economic Analysis Department",
                    "Departemnt of Economics, University of Notre Dame and NBER")
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
            for(id in seq_along(markers)) {
                ind <- markers == markers[[id]]
                aff_pattern <- paste0(",", markers[[id]], 
                                      "(?: *Corresponding author: *)?",
                                      "([^,]*?),([^,]*?),")
                stopifnot(grepl(aff_pattern, all_text))
                ap <- paste0(amp, ".*", aff_pattern, ".*$")
                aff <- sub(ap, " \\1| \\2", all_text)
                aff <- trimws(strsplit(aff, "\\|")[[1]])
                if (aff[[2]]==BoCE) {
                    row$language <- "English"
                    affils[ind] <- paste("BoC", aff[[1]])
                } else if (aff[[2]] == BoCF) {
                    row$language <- "Français"
                    affils[ind] <- paste("BoC", aff[[1]])
                } else if (grepl("(university|bank)", aff[[1]], ignore.case=TRUE)) {
                    affils[ind] <- aff[[1]]
                } else {
                    affils[ind] <- paste(aff, collapse=", ")
                }
            }
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

# parse_one_author <- function(row, kind, author, pdftext) {
#     BoCE <- "Bank of Canada"
#     BoCF <- "Banque du Canada"
#     if (kind=="swp" && author == "Warren E. Weber") {
#         row$language <- "English"
#         row$affils <- paste("BoC", "Visiting Scholar at Currency Department")
#     } else if (kind=="swp" && row$id == "2012-17") {
#         row$language <- "English"
#         row$affils <- paste("BoC", "Funds Management and Banking Department")
#     } else if (kind=="sdp" && row$id == "2014-6") {
#         row$language <- "English"
#         row$affils <- paste("BoC", "Governor")
#     } else {
#         all_text <- iconv(paste(pdftext, collapse=","),
#                           to="ASCII//TRANSLIT")
#         author_pattern <- iconv(fuzzy_author_pattern(author), to="ASCII//TRANSLIT")
#         # note: (.*?) - the ? means shortest match
#         pattern <- paste0("^.*", author_pattern, 
#                           "\\s*,(.*?),\\s*", BoCE, ".*$")
#         if (grepl(pattern, all_text)) {
#             row$language <- "English"
#         } else {
#             pattern <- paste0("^.*", fuzzy_author_pattern,
#                               "\\s*,(.*?),\\s*", BoCF, ".*$")
#             if (grepl(pattern, all_text)) {
#                 row$language <- "Français"
#             } else {
#                 stop("pattern does not match")
#             }
#         }
# 
#         row$affils <- paste("BoC", trimws(sub(pattern, "\\1", all_text)))
#     }
#     return(row)
# }
 
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



#     if(length(authors)==1) {
#         row <- parse_one_author(row, kind, authors[[1]], pdftext)
#         cat("    ", row$id)
#         cat("\t", row$language, "\n", "\t", authors[[1]], "->",  row$affils, "\n")
#     } else if (length(authors) == 2) {
#         cat("    ", row$id)
# 
#         BoCE <- "Bank of Canada"
#         BoCF <- "Banque du Canada"
#         
#         # Typos in names
#         if(kind=="swp" && row$id=="2016-60") 
#             authors[[1]] <- "Teodoara Paligorova"
#         if(kind=="swp" && row$id=="2017-6") 
#             authors[[2]] <- "Josef Schoth"
#         if(kind=="swp" && row$id=="2010-13") 
#             authors[[2]] <- "Yinan Zhang"
#         if(kind=="swp" && row$id=="2015-8")
#             authors[[1]] <- "Jose Garralda"
# 
#         # Special cases
#         if(kind=="swp" && row$id=="2016-30") {
#             row$language <- "English"
#             affils <- c("European Central Bank", 
#                         "BoC Economic and Financial Research")
#         } else if (kind=="swp" && any(row$id==c("2016-32","2016-36"))) {
#             row$language <- "English"
#             affils <- c("BoC International Economic Analysis Department",
#                         "Departemnt of Economics, University of Notre Dame and NBER")
#         } else {
#             # general case
#             all_text <- iconv(paste(pdftext, collapse=","),
#                               to="ASCII//TRANSLIT")
#             fuzzy_author_pattern <- iconv(gsub("\\s+", " ?\\\\S* ?\\\\S* ?", 
#                                                authors),
#                                           to="ASCII//TRANSLIT")
#             # note: (.*?) - the ? means shortest match
#             markers_pattern <- paste0("^.*", 
#                               fuzzy_author_pattern[[1]],
#                               "(\\d?),? ?and ?,?",
#                               fuzzy_author_pattern[[2]],
#                               "(\\d?).*$")
#             stopifnot(grepl(markers_pattern, all_text))
#             markers <- strsplit(sub(markers_pattern, "\\1 | \\2", 
#                                     all_text), "\\|")[[1]]
#             markers <- lapply(markers, trimws)
#             
#             if (all(nchar(markers)==0)) {
#                 # two authors from the Bank from the same department
#                 dept_pattern <- paste0("^.*", 
#                               fuzzy_author_pattern[[1]],
#                               "\\d? and ",
#                               fuzzy_author_pattern[[2]],
#                               "\\d?\\s*,(.*?),\\s*", BoCE, ".*$")
#                 if (grepl(dept_pattern, all_text)) {
#                     row$language <- "English"
#                 } else {
#                     dept_pattern <- sub(BoCE, BoCF, dept_pattern)
#                     if (grepl(pattern, all_text)) {
#                         row$language <- "Français"
#                     } else {
#                         stop("pattern does not match")
#                     }
#                 }
#     
#                 dept <- paste("BoC", trimws(sub(dept_pattern, "\\1", all_text)))
#                 affils <- c(dept, dept)
#             } else {
#                 # stop("2")
#                 row$language <- "English"
#                 affils <- c("", "")
#                 for(id in seq_along(authors)) {
#                     aff_pattern <- paste0(",", markers[[id]], "([^,]*?),([^,]*?),")
#                     stopifnot(grepl(aff_pattern, all_text))
#                     aff <- sub(paste0("^.*",aff_pattern,".*$"), 
#                                "\\1 | \\2", all_text)
#                     aff <- trimws(strsplit(aff, "\\|")[[1]])
#                     if (aff[[2]]==BoCE) {
#                         row$language <- "English"
#                         affils[[id]] <- paste("BoC", aff[[1]])
#                     } else if (aff[[2]] == BoCF) {
#                         row$language <- "Français"
#                         affils[[id]] <- paste("BoC", aff[[1]])
#                     } else {
#                         affils[[id]] <- paste(aff, collapse=",")
#                     }
#                 }
#             }
#         }
# 
#         cat("\t", row$language, "\n")
#         row$affils <- paste(affils, collapse="|")
#         for(i in seq_along(authors)) 
#             cat("\t", authors[[i]], "->", affils[[i]], "\n")
#     } else {
#         cat(row$id, ": ", authors, "\n")
#         stop("That's all folks")
#     }
# 
#     return(row)
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

