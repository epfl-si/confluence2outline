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

if ("--small-sample" %in% cmdline) {
    confluence$pages <- test.page
    confluence$attachments <- confluence$attachments %>%
        weed_unused_attachments(confluence$pages)
}

if ("--skip-attachments" %in% cmdline) {
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

if (! ("--skip-zip" %in% cmdline)) {
    source_python("zip-streams.py")
    zip.from <- ZipSource(archive_path)
    zip.to <- ifelse("--small-sample" %in% cmdline,
                     "outline-SMALL.zip",
                     "outline.zip") %>%
        ZipSink()
    if (! ("--skip-attachments" %in% cmdline)) {
        rewrite.attachments(outline$attachments, zip.from, zip.to)
    }
    outline_json %>%
        zip.to$add(as_filename = outline$meta.filename)
    zip.to$close()
}
