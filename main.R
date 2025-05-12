# install.packages("tidyverse")
# install.packages("getopt")

library(tidyverse)

if (interactive()) {
    opts <- list("skip-install" = TRUE)
} else {
    opts <- matrix(byrow=TRUE, ncol=4, c(
      "skip-install"       , NA, 0, "logical"
    )) %>%
        getopt::getopt()
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

############################### Test #################################

test.page <-
    pages %>%
    filter(is.latest &
           str_detect(body, "quasi-services"))

test.page %>% pull(body) %>% write("test/data/services-etc.xml")

########################## Transform, Load ###########################

source("outline.R")

source_python("zip-streams.py")
zip.from <- ZipSource(archive_path)
zip.to <- ZipSink("outline.zip")
{
    attachments_ETL <- outline.attachments %>%
        split(.$confluence_zip_path)
    zip.from$files() %>%
        iterate(function(zip.file) {
            attachment <- attachments_ETL[[zip.file$path]]
            if (length(attachment)) {
                as_filename <- attachment$key
                print(as_filename)
                zip.to$add(zip.file, as_filename = as_filename)
            }
        })
}

metadata.zip_filename <- "ISAS-FSD.json"

list(attachments = outline.attachments.meta) %>%
    jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>%
    zip.to$add(as_filename = metadata.zip_filename)

zip.to$close()
