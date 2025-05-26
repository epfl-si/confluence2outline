# install.packages("data.tree")

library(data.tree)

#' Load the documents from the JSON file inside an Outline Zip,
#' for testing purposes.
load.outline <- function (.zip_path, json_filename = NULL) {
    if (is.null(json_filename)) {
        json_filename <-
            .zip_path %>%
            archive::archive() %>%
            filter(str_ends(path, ".json") &
                   ! path %in% c("Welcome.json", "metadata.json")) %>%
            arrange(desc(size)) %>%
            { .[1,] } %>%
            pull(path)
    }

    outline.dump <-
        archive::archive_read(.zip_path, file = json_filename) %>%
        jsonlite::fromJSON(simplifyDataFrame = FALSE)

    outline.documents.tree <-
        outline.dump$collection$documentStructure %>%
        list(children = .) %>%
        FromListExplicit(nameName = "id")

    outline.document.structure <-
        outline.documents.tree %>%
        ToDataFrameNetwork("name", "url", "icon", "color", "title") %>%
        rename(parent = from, id = name) %>% {
            stopifnot("`to` is the node name (by construction)" =
                          all(.$name == .$to))
            select(., -to)
        }

    eliminate.identical.column <- function(.df, col1, col2) {
        col1 <- ensym(col1)
        col2 <- ensym(col2)
        col1.name <- rlang::as_string(col1)
        col2.name <- rlang::as_string(col2)

        ## ?stopifnot says it's bad style to do computed calls,
        ## so we do if (! (...)) { stop() } instead:
        if (! (.df %>%
               filter(is.na(!!col1) != is.na(!!col2)) %>%
               nrow == 0) ) {
            paste0(col1.name, " and ", col2.name,
                   " should have NAs in the same places") %>%
                stop()
        }

        if (! (.df %>%
               filter(! is.na(!!col1)) %>%
               { all(pull(., !!col1) == pull(., !!col2)) }) ) {
            paste0(col1.name, " and ", col2.name,
                   " should have identical (non-NA) values") %>%
                stop()
        }

        .df %>% select(-!!col2)
    }


    outline.documents <-
        outline.dump$documents %>%
        tibble::enframe() %>%
        unnest_wider(value) %>%
        {
            stopifnot(all(.$name == .$id))
            stopifnot(all(.$name == .$id))
            select(., -name)
        } %>%
        as_tibble() %>% {
            by <- join_by(id)

            stopifnot(
                "All items in `$documents` are also in `$documentStructure`" =
                    anti_join(., outline.document.structure, by = by) %>%
                    nrow == 0)

            stopifnot(
                "All nodes in `$documentStructure` are also in `$documents`" =
                    anti_join(outline.document.structure, ., by = by) %>%
                    nrow == 0)

            left_join(., outline.document.structure,
                      by = by, suffix = c("", ".s"))
        } %>%
        eliminate.identical.column(., icon, icon.s) %>%
        eliminate.identical.column(color, color.s) %>%
        eliminate.identical.column(title, title.s) %>%
        mutate(parent = ifelse(parent == "Root", as.character(NA), parent)) %>%
        eliminate.identical.column(parentDocumentId, parent)

    list(.dump = outline.dump,
         documents = outline.documents,
         tree = outline.documents.tree)
}
