# install.packages("tidyverse")
# install.packages("getopt")

library(tidyverse)

if (interactive()) {
    opts <- list("skip-install" = TRUE)
    archive_path <- "/Users/quatrava/Downloads/ISAS-FSD-idevfsd-2025-03-28-11-36-24-881.xml.zip"
} else {
    opts <- matrix(byrow=TRUE, ncol=4, c(
      "dump-after-extract" , NA, 0, "logical",
      "from"               , NA, 2, "character",
      "skip-attachments"   , NA, 0, "logical",
      "skip-install"       , NA, 0, "logical",
      "skip-zip"           , NA, 0, "logical",
      "small-sample"       , NA, 0, "logical"
    ))
    opts <- getopt::getopt(opts)
    archive_path <- opts$from
}

############################# Make ready #############################

source("install.R")
source("python.R")

if (setup_pyenv()$changed) {
    if (is.null(opts$`skip-install`)) {
        install_from_comments(dry_run = FALSE)
    }
    install_python_dependencies()
    if (interactive()) {
        install_python_dev_dependencies()
    }
}

############################## Extract ###############################

undump.filename <- "extracted.Rdata"
dump.after.extract <- ! is.null(opts$`dump-after-extract`)

if ((! interactive()) & file.exists(undump.filename)
    & ! dump.after.extract) {
    confluence <- readRDS(file = undump.filename)
} else {
    source("confluence.R")
    confluence <- extract.confluence(archive_path = archive_path)

    if (dump.after.extract) {
        confluence %>%
            saveRDS(file = undump.filename)
        quit()
    }
}

confluence_everything <- list(pages = confluence$pages,
                              attachments = confluence$attachments)

confluence$pages <- confluence$pages %>% filter(is.latest & contentStatus == "current")

weed_unused_attachments <- function (attachments, pages) {
    attachments %>%
        filter(containerContent.Page %in% pages$page_id)
}

confluence$attachments <- confluence$attachments %>%
    weed_unused_attachments(confluence$pages)

############################### Test #################################

source("tests.R")
o <- load.outline("import-Outline/ISAS-FSD-export.json.zip")

test.page <-
    confluence$pages %>%
    filter(str_detect(body, "quasi-services"))

test.page %>% pull(body) %>% write("test/data/services-etc.xml")

if (! is.null(opts$`small-sample`)) {
    confluence$pages <- local({
        restricted.pages <- sample_frac(confluence$pages, 0.2)
        if (! (test.page$page_id %in% restricted.pages$page_id)) {
            restricted.pages <- restricted.pages %>%
                bind_rows(test.page)
        }
        sans_na <- function(vec) { vec[! is.na(vec)] }

        while(! all( (restricted.pages$parent.Page %>% sans_na())
                    %in% restricted.pages$page_id ) )
        {
            print(glue::glue("Completing parents of { nrow(restricted.pages) } rows"))
            restricted.pages <-
                confluence$pages %>%
                filter(page_id %in% c(
                                        restricted.pages$page_id,
                                        restricted.pages$parent.Page %>% sans_na()))
        }
        restricted.pages
    })
    confluence$attachments <- confluence$attachments %>%
        weed_unused_attachments(confluence$pages)
}

if (! is.null(opts$`skip-attachments`)) {
    confluence$attachments <- confluence$attachments %>% filter(FALSE)
}

############################ Transform ###############################

source("outline.R")
outline <- transform(archive_path, confluence)

outline_json <- outline$meta %>%
    jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)

############################ Test more ###############################

outline_json %>%
    write(outline$meta.filename)

if (interactive()) {
    source_python("test/schema.py")
    stopifnot("outline$meta matches the Outline JSON schema" =
                  validate_json_outline_file(outline$meta.filename))
}

############################### Load #################################

if (is.null(opts$`skip-zip`)) {
    source_python("zip-streams.py")
    zip.from <- ZipSource(archive_path)
    zip.to <- ifelse(is.null(opts$`small-sample`),
                 "outline.zip",
                 "outline-SMALL.zip") %>%
        ZipSink()
    if (is.null(opts$`skip-attachments`)) {
        rewrite.attachments(outline$attachments, zip.from, zip.to)
    }
    outline_json %>%
        zip.to$add(as_filename = outline$meta.filename)
    zip.to$close()
}
