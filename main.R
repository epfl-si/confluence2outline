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
# To be continued...
