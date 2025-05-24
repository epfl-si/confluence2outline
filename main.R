cmdline <- commandArgs(trailingOnly=TRUE)

if (interactive()) {
    cmdline <- c(cmdline, "--skip-install")
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

source("confluence.R")

############################### Test #################################

test.page <-
    pages %>%
    filter(is.latest &
           str_detect(body, "quasi-services"))

test.page %>% pull(body) %>% write("test/data/services-etc.xml")

########################## Transform, Load ###########################
# To be continued...
