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

confluence_id <- function (node_or_nodeset) {
    xml_find_first(node_or_nodeset, 'id') %>%
        xml_text
}

#' Format the classless “props” of each node in a nodeset, into a tibble
#'
#' @param objects_nodeset An XML nodeset (*not* a list of many
#'     nodesets)
#' @return A tibble with as many rows as there are objects in
#'     objects_nodeset. The columns of the returned tibble are the XML
#'     “name” attributes of any `property` sub-nodes that do *not*
#'     have a `class` attribute; and the values are the XML texts
#'     found within said property nodes.
props_tibble <- function(objects_nodeset) {
    objects_nodeset %>%
    xml_find_all('property[not(@class)]', flatten=FALSE) %>%
    tibble(row = seq_along(.),
           props = .) %>%
        rowwise() %>%
        reframe(
            row = row,
            pkeys = xml_attr(props, 'name'),
            pvals = xml_text(props)) %>%
        pivot_wider(names_from = pkeys, values_from = pvals) %>%
        select(-row)
}

#' Format the “props” that have a `class` for each node in a nodeset, into a tibble
#'
#' @param objects_nodeset An XML nodeset (*not* a list of many
#'     nodesets)
#' @return A tibble with as many rows as there are objects in
#'     objects_nodeset. The columns of the returned tibble are named
#'     as `foo.Bar`, where `foo` (e.g. "content") is the `name`
#'     attribute, and `Bar` is the `class` attribute, of any
#'     `property` sub-nodes that have a `class` attribute; and the
#'     values are the XML texts found within said property nodes.
classful_props_tibble <- function(objects_nodeset) {
    objects_nodeset %>%
    xml_find_all('property[@class]', flatten=FALSE) %>%
    tibble(row = seq_along(.),
           props = .) %>%
        rowwise() %>%
        reframe(
            row = row,
            pnames = xml_attr(props, 'name'),
            pclasses = xml_attr(props, 'class'),
            pvals = xml_text(props)) %>%
        mutate(pkeys=paste(pnames, pclasses, sep="."), .keep = "unused") %>%
        pivot_wider(names_from = pkeys, values_from = pvals) %>%
        select(-row)
}

## Again, stuff like
##
##      object_pages <- entities %>%
##          filter(element == "object" &
##                 class == "Page" &
##                 package == "com.atlassian.confluence.pages") %>%
##          mutate(id = purrr::map_chr(node, ~ xml_find_first(.x, "id") %>% xml_text))
##
## would be way too s  l  o   w.

page_versions <- {
    ns <- entities_xml %>% xml_find_all('//object[@class="Page"]')
    tibble(content_id = ns %>% confluence_id) %>%
        mutate(ns %>% props_tibble) %>%
        mutate(ns %>% classful_props_tibble) %>%
        mutate(ns %>%
               xml_find_all(c('collection[@name="contentProperties"]',
                              'element[@class="ContentProperty"]',
                              'id[@name="id"]') %>%
                            paste(collapse = "/"),
                            flatten = FALSE) %>%
               tibble(ids = .) %>% rowwise %>%
               transmute(content_property_ids = xml_text(ids) %>% list))
}

content_properties <- {
    ns <- entities_xml %>% xml_find_all('//object[@class="ContentProperty"]')
    tibble(property_id = ns %>% confluence_id) %>%
        mutate(ns %>% props_tibble) %>%
        mutate(ns %>% classful_props_tibble)
}
