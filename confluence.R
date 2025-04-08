# install.packages("archive")

library(tidyverse)
library(archive)
library(xml2)

archive_path <- "/Users/quatrava/Downloads/ISAS-FSD-idevfsd-2025-03-28-11-36-24-881.xml.zip"

entities_xml <-
    archive::archive_read(archive_path, file = "entities.xml") %>%
    read_xml

entities <-
    entities_xml %>%
    {
        ## xml2's nodeset type creates a de facto constraint on the
        ## magrittr pipeline steps (if we want performance). Tempted
        ## as we might be for “elegance” reasons, we don't want to
        ## return a single-column tibble of nodes here, and defer a
        ## bunch of slow `purrr::map_chr` further down below. (Note
        ## that xml2's `as_list` would be even slower than that.)
        nodeset <- xml_find_all(., "//*")
        tibble(
            ## Okay, I got some help from ChatGPT on this one.
            node = purrr::map(seq_along(nodeset), ~ nodeset[[.x]]),
            element = nodeset %>% xml_name,
            class = nodeset %>% xml_attr("class"),
            package = nodeset %>% xml_attr("package"),
            name = nodeset %>% xml_attr("name"))
    }

summary <- entities %>%
    count(element, package, class, name) %>%
    arrange(n)
