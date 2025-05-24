cmdline <- commandArgs(trailingOnly=TRUE)

if (interactive()) {
    cmdline <- c(cmdline, "--skip-install", "--skip-zip")
}

############################# Make ready #############################

source("install.R")
if (! ("--skip-install" %in% cmdline)) {
    install_from_comments(dry_run = FALSE)
}

source("python.R")
setup_pyenv()
if (! ("--skip-install" %in% cmdline)) {
    install_python_dependencies()
}

############################## Extract ###############################

archive_path <- "/Users/quatrava/Downloads/ISAS-FSD-idevfsd-2025-03-28-11-36-24-881.xml.zip"
source("confluence.R")
confluence <- extract.confluence(archive_path = archive_path)

############################### Test #################################

test.page <-
    confluence$pages %>%
    filter(is.latest &
           str_detect(body, "quasi-services"))

test.page %>% pull(body) %>% write("test/data/services-etc.xml")

if ("--small-sample" %in% cmdline) {
    confluence$pages <- test.page
    confluence$attachments <- confluence$attachments %>%
        filter(containerContent.Page == test.page$page_id)
}

############################ Transform ###############################

source("outline.R")
outline <- transform(archive_path, confluence)

############################### Load #################################

if ("--skip-zip" %in% cmdline) {
    outline$meta %>%
        jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>%
        write(outline$meta.filename)
} else {
    source_python("zip-streams.py")
    zip.from <- ZipSource(archive_path)
    zip.to <- ifelse("--small-sample" %in% cmdline,
                     "outline-SMALL.zip",
                     "outline.zip") %>%
        ZipSink()
    rewrite.attachments(outline$attachments, zip.from, zip.to)
    outline$meta %>%
        jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>%
        zip.to$add(as_filename = outline$meta.filename)
    zip.to$close()
}
