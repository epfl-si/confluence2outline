cmdline <- commandArgs(trailingOnly=TRUE)

if (interactive()) {
    cmdline <- c(cmdline, "--skip-zip")
}

############################# Make ready #############################

source("install.R")
source("python.R")
if (setup_pyenv()$changed) {
    if (! ("--skip-install" %in% cmdline)) {
        install_from_comments(dry_run = FALSE)
    }
    install_python_dependencies()
    if (interactive()) {
        install_python_dev_dependencies()
    }
}

############################## Extract ###############################

archive_path <- "/Users/quatrava/Downloads/ISAS-FSD-idevfsd-2025-03-28-11-36-24-881.xml.zip"
source("confluence.R")
confluence <- extract.confluence(archive_path = archive_path)

############################### Test #################################

source("tests.R")
o <- load.outline("import-Outline/ISAS-FSD-export.json.zip")

test.page <-
    confluence$pages %>%
    filter(is.latest &
           str_detect(body, "quasi-services"))

test.page %>% pull(body) %>% write("test/data/services-etc.xml")

if ("--small-sample" %in% cmdline) {
    confluence_orig <- list(pages = confluence$pages,
                            attachments = confluence$attachments)
    confluence$pages <- local({
        restricted.pages <- sample_frac(confluence$pages, 0.2)
        if (! (test.page$page_id %in% restricted.pages$page_id)) {
            restricted.pages <- restricted.pages %>%
                bind_rows(test.page)
        }
        sans_na <- function(vec) { vec[! is.na(vec)] }
    
        while(! (
            all( (restricted.pages$parent.Page %>% sans_na())
                %in% restricted.pages$page_id )
            &
            all( (restricted.pages$latest.version %in% restricted.pages$page_id ) )
        )) {
            print(glue::glue("Completing parents and latest.versions of {
                                  nrow(restricted.pages) } rows"))
            restricted.pages <-
                confluence$pages %>%
                filter((page_id %in% c(
                                         restricted.pages$page_id,
                                         restricted.pages$parent.Page %>% sans_na(),
                                         restricted.pages$latest.version
                                         )))
        }
        restricted.pages
    })
    confluence$attachments <- confluence$attachments %>%
        filter(containerContent.Page %in% confluence$pages$page_id)
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

if (! ("--skip-zip" %in% cmdline)) {
    source_python("zip-streams.py")
    zip.from <- ZipSource(archive_path)
    zip.to <- ifelse("--small-sample" %in% cmdline,
                     "outline-SMALL.zip",
                     "outline.zip") %>%
        ZipSink()
    rewrite.attachments(outline$attachments, zip.from, zip.to)
    outline_json %>%
        zip.to$add(as_filename = outline$meta.filename)
    zip.to$close()
}
