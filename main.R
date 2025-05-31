# install.packages("tidyverse")
# install.packages("getopt")

library(tidyverse)

if (interactive()) {
    opts <- list("skip-install" = TRUE)
    archive_path <- "/Users/quatrava/Downloads/ISAS-FSD-idevfsd-2025-03-28-11-36-24-881.xml.zip"
} else {
    opts <- matrix(byrow=TRUE, ncol=4, c(
      "from"               , NA, 2, "character",
      "skip-install"       , NA, 0, "logical",
      "skip-zip"           , NA, 0, "logical",
      "small-sample"       , NA, 0, "logical"
    ))
    opts <- getopt::getopt(opts)
    archive_path <- opts$from
}

############################# Make ready #############################

source("install.R")
if (is.null(opts$`skip-install`)) {
    install_from_comments(dry_run = FALSE)
}

source("python.R")
setup_pyenv()
if (is_null(opts$`skip-install`)) {
    install_python_dependencies()
}

############################## Extract ###############################

source("confluence.R")
confluence <- extract.confluence(archive_path = archive_path)

############################### Test #################################

test.page <-
    confluence$pages %>%
    filter(is.latest &
           str_detect(body, "quasi-services"))

test.page %>% pull(body) %>% write("test/data/services-etc.xml")

if (! is.null(opts$`small-sample`)) {
    confluence$pages <- test.page
    confluence$attachments <- confluence$attachments %>%
        filter(containerContent.Page == test.page$page_id)
}

############################ Transform ###############################

source("outline.R")
outline <- transform(archive_path, confluence)

outline_json <- outline$meta %>%
    jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)

############################### Load #################################

if (! is.null(opts$`skip-zip`)) {
    outline_json %>%
        write(outline$meta.filename)
} else {
    source_python("zip-streams.py")
    zip.from <- ZipSource(archive_path)
    zip.to <- ifelse(is.null(opts$`small-sample`),
                 "outline.zip",
                 "outline-SMALL.zip") %>%
        ZipSink()
    rewrite.attachments(outline$attachments, zip.from, zip.to)
    outline_json %>%
        zip.to$add(as_filename = outline$meta.filename)
    zip.to$close()
}
