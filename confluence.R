# install.packages("archive")
# install.packages("glue")

library(tidyverse)
library(archive)
library(xml2)
library(bit64)

extract.confluence <- function(archive_path) {
    entities_xml <-
        archive::archive_read(archive_path, file = "entities.xml") %>%
        read_xml

    entities <- local({
        ## xml2's nodeset type creates a de facto constraint on the
        ## magrittr pipeline steps (if we want performance). Tempted
        ## as we might be for “elegance” reasons, we don't want to
        ## return a single-column tibble of nodes here, and defer a
        ## bunch of slow `purrr::map_chr` further down below. (Note
        ## that xml2's `as_list` would be even slower than that.)
        nodeset <- xml_find_all(entities_xml, "//*")
        tibble(
            ## Okay, I got some help from ChatGPT on this one.
            node = purrr::map(seq_along(nodeset), ~ nodeset[[.x]]),
            element = nodeset %>% xml_name,
            class = nodeset %>% xml_attr("class"),
            package = nodeset %>% xml_attr("package"),
            name = nodeset %>% xml_attr("name"))
    })

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
            select(-row) %>%
            mutate(across(any_of(c("position")) |
                          ends_with("version", ignore.case = TRUE),
                          as.integer),
                   across(any_of(c("longValue")),
                          as.integer64),
                   across(any_of(c("contentStatus", "relationName", "targetType")),
                          as.factor),
                   across(ends_with("date", ignore.case = TRUE),
                          parse_datetime))
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

    content_properties <- local({
        ns <- entities_xml %>% xml_find_all('//object[@class="ContentProperty"]')
        tibble(property_id = ns %>% confluence_id) %>%
            mutate(ns %>% props_tibble) %>%
            mutate(ns %>% classful_props_tibble)
    })

    ## Again, stuff like
    ##
    ##      object_pages <- entities %>%
    ##          filter(element == "object" &
    ##                 class == "Page" &
    ##                 package == "com.atlassian.confluence.pages") %>%
    ##          mutate(id = purrr::map_chr(node, ~ xml_find_first(.x, "id") %>% xml_text))
    ##
    ## would be way too s  l  o   w.

    page_versions <- local({
        ns <- entities_xml %>% xml_find_all('//object[@class="Page"]')
        page_versions <-
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
        ## page_versions$content_property_ids (as a “multivalued foreign key”)
        ## ought to contain the same information as
        ## content_properties[c("content.Page", "property_id")]:
        stopifnot(
            "page_versions$content_property_ids is redundant" = {
                relation1 <-
                    page_versions %>%
                    transmute(content_id, property_id = content_property_ids) %>%
                    unnest_longer(property_id)
                relation2 <-
                    content_properties %>%
                    filter(! is.na(content.Page)) %>%
                    transmute(content_id = content.Page, property_id)
                by <- join_by(content_id, property_id)
                anti_join(relation1, relation2, by = by) %>% nrow == 0 &&
                    anti_join(relation2, relation1, by = by) %>% nrow == 0
            })
        page_versions <- page_versions %>% select(-content_property_ids)
        ## Pages ought to be grouped into version histories by their
        ## `originalVersion.Page`, which needs a little data-cleaning:
        stopifnot("No page version has itself as the original" =
                      page_versions %>%
                      filter(content_id == originalVersion.Page) %>%
                      nrow == 0)
        page_versions <-
            page_versions %>%
            mutate(.keep = "unused",   # Except as shown below
                   page_id = content_id,
                   is.original = is.na(originalVersion.Page),
                   originalVersion = coalesce(originalVersion.Page, page_id))
        stopifnot("versions should be unique per version group" =
                      ## (... except perhaps for drafts)
                      page_versions %>%
                      filter(contentStatus != "draft") %>%
                      group_by(originalVersion) %>%
                      summarize(n = n(), n_distinct = n_distinct(version)) %>%
                      filter(n != n_distinct) %>%
                      nrow == 0)
        stopifnot("Current version ought to be the last version" =
                      page_versions %>%
                      group_by(originalVersion) %>%
                      summarize(last = max(version),
                                original = tibble(version, is.original) %>%
                                    filter(is.original) %>%
                                    pull(version)) %>%
                      filter(last != original)  %>%
                      nrow == 0)
        stopifnot("page.Parent referential integrity" =
                      page_versions  %>%
                      filter(! is.na(parent.Page)) %>%
                      anti_join(page_versions,
                                by = join_by(parent.Page == page_id)) %>%
                      nrow == 0)
        page_versions %>% rename(is.latest = is.original,
                                 latest.version = originalVersion)
    })

    bodies <- local({
        ns <- entities_xml %>% xml_find_all('//object[@class="BodyContent"]')
        bodies <-
            tibble(body_id = ns %>% confluence_id) %>%
            mutate(ns %>% props_tibble) %>%
            mutate(body = body %>%
                       str_replace_all("]] >", "]]>") %>%
                       paste0('<confluence-body>', ., '</confluence-body>')) %>%
            mutate(.keep = "unused",
                   type = case_when(
                       bodyType == 0 ~ "SpaceDescription",
                       bodyType == 1 ~ "CustomContentEntityObject",
                       bodyType == 2 ~ "PageEtc") %>%
                       as_factor) %>%
            mutate(ns %>% classful_props_tibble)
        stopifnot("All bodies have a type" =
                      bodies %>%
                      filter(is.na(type)) %>%
                      nrow == 0)
        stopifnot("Type of bodies pointing to a `content.SpaceDescription`" =
                      bodies %>%
                      filter(! is.na(content.SpaceDescription)) %>%
                      filter(type != "SpaceDescription") %>%
                      nrow == 0)
        stopifnot("Type of bodies pointing to a `content.CustomContentEntityObject`" =
                      bodies %>%
                      filter(! is.na(content.CustomContentEntityObject)) %>%
                      filter(type != "CustomContentEntityObject") %>%
                      nrow == 0)
        bodies
    })

    page_bodies <- bodies %>%
        filter(! is.na(content.Page)) %>%
        rename(page_id = content.Page) %>%
        relocate(page_id)

    pages <- local({
        by <- join_by(page_id)
        stopifnot("Page-like bodies should have a match in page_versions" =
                      page_bodies %>%
                      anti_join(page_versions, by = by) %>%
                      nrow == 0)
        ## ⚠ Some pages don't have a body!!
        page_versions %>%
            left_join(page_bodies, by = by)
    })

    stopifnot("parent.Page referential integrity" =
                  pages %>%
                  filter(! is.na(parent.Page)) %>%
                  anti_join(pages, by = join_by(parent.Page == page_id)) %>%
                  nrow == 0)

    user2content <- local({
        ns <- entities_xml %>%
            xml_find_all('//object[@class="User2ContentRelationEntity"]')
        tibble(u2c_id = ns %>% confluence_id) %>%
            mutate(ns %>% props_tibble) %>%
            mutate(ns %>% classful_props_tibble)
    })

    users <- local({
        ns <- entities_xml %>%
            xml_find_all('//object[@class="ConfluenceUserImpl"]')
        tibble(user_key = ns %>% confluence_id) %>%
            mutate(ns %>% props_tibble)
    })

    attachments <- local({
        ns <- entities_xml %>%
            xml_find_all('//object[@class="Attachment"]')
        attachments_xml <- tibble(attachment_id = ns %>% confluence_id) %>%
            mutate(ns %>% props_tibble) %>%
            mutate(ns %>% classful_props_tibble) %>%
            mutate(
                originalAttachmentId = coalesce(originalVersion.Attachment, attachment_id),
                is.original = is.na(originalVersion.Attachment),
                confluence_zip_path = glue::glue("attachments/{containerContent.Page}/{originalAttachmentId}/{version}"),
                ext = title %>% tools::file_ext() %>% as.factor) %>%
            select(-originalVersion.Attachment)
        confluence_files <- archive(archive_path)
        by <- join_by(confluence_zip_path == path)
        stopifnot("No lost attachments" =
                      attachments_xml %>%
                      anti_join(confluence_files, by = by) %>%
                      nrow == 0)
        stopifnot("No orphan files in Zip" =
                      confluence_files %>%
                      anti_join(attachments_xml,
                                by = join_by(path == confluence_zip_path)) %>%
                      filter(! (path %in% c("entities.xml",
                                            "exportDescriptor.properties"))) %>%
                      nrow == 0)
        attachments <- attachments_xml %>% left_join(confluence_files, by = by)
        media_type_properties <- content_properties %>%
            filter(name == "MEDIA_TYPE")
        stopifnot("Media types are for attachments only" =
                      media_type_properties %>%
                      filter(is.na(content.Attachment)) %>%
                      nrow == 0)
        media_types <-
            media_type_properties %>%
            mutate(.keep = "none",
                   attachment_id = content.Attachment,
                   media.type = stringValue %>% as.factor)
        stopifnot("All attachments should have a media type" =
                      attachments %>%
                      anti_join(media_types, by = join_by(attachment_id)) %>%
                      nrow == 0)
        attachments_and_types <-
            attachments %>%
            left_join(media_types, by = join_by(attachment_id))
        stopifnot("`version` should be unique per attachment group" =
                      attachments_and_types %>%
                      group_by(originalAttachmentId) %>%
                      summarize(n = n(), n_distinct = n_distinct(version)) %>%
                      filter(n != n_distinct) %>%
                      nrow == 0)
        stopifnot("“Original” attachment ID should in reality be the latest ID" =
                      attachments_and_types %>%
                      group_by(originalAttachmentId) %>%
                      summarize(last = max(version),
                                original = tibble(version, is.original) %>%
                                    filter(is.original) %>%
                                    pull(version)) %>%
                      filter(last != original)  %>%
                      nrow == 0)
        attachments_and_types %>%
            rename(is.latest = is.original,
                   latest.attachment_id = originalAttachmentId)
    })

    list(entities_xml = entities_xml,
         pages = pages,
         page_bodies = page_bodies,
         attachments = attachments)
}
