# install.packages("glue")
# install.packages("uuid")
# install.packages("data.tree")

library(data.tree)
library(stringr)

deterministic.uuid <- function (char_v, ...) {
    ## With the `uuid` package, only UUIDs (deterministically) beget
    ## UUIDs for some reason:
    namespace <- "6e28e697-bed4-4e50-b6e5-4efc31b478b9"
    for (salt in rlang::list2(...)) {
        namespace <- uuid::UUIDfromName(namespace, salt)
    }

    uuid::UUIDfromName(namespace, char_v)
}

transform.attachments <- function (.confluence_attachments, outline_documents, uuid_salt) {
    uploads.prefix <- "uploads/restored-from-Confluence"
    attachments <-
        .confluence_attachments %>%
        mutate(attachment_uuid = deterministic.uuid(attachment_id,
                                                    "attachment_id", uuid_salt),
               outline.key = glue::glue("{ uploads.prefix }/{ attachment_uuid }/{ title }")) %>%
        relocate(attachment_uuid, outline.key) %>%
        select(-attachment_id)

    by = join_by(containerContent.Page == page_id)
    stopifnot("All attachments belong to a document" =
                  attachments %>%
                  anti_join(outline_documents, by = by) %>%
                  nrow == 0)

    meta <- attachments %>%
        left_join(
            outline_documents %>% select(page_id, document_uuid),
            by = by) %>%
        transmute(id = attachment_uuid, documentId = document_uuid,
                  contentType = media.type, size, name = title) %>%
        split(.$id) %>%
        map(~ as.list(.x))

    rlang::env(tibble = attachments,
               meta = meta)
}

transform.documentMetadata <- function (.confluence_pages, uuid_salt) {
    documents <- .confluence_pages %>%
        mutate(document_uuid = deterministic.uuid(
                   page_id, "document_id", uuid_salt))

    document.tibble <-
        documents %>%
        # We already checked null-anti-join-ness in confluence.R:
        left_join(documents %>%
                  mutate(.keep = "none",
                         parent.Page = page_id, parent_uuid = document_uuid),
                  by = join_by(parent.Page))

    match.emoji <- paste0(
        "(?:\\p{Emoji_Presentation}|\\p{Extended_Pictographic})",
        "(?:\\p{Regional_Indicator}|\\p{Emoji_Modifier})*")

    document.tree <-
        document.tibble %>%
        mutate(.keep="none",
               id = document_uuid,
               parent = replace_na(parent_uuid, "__ROOT__"),
               title,
               icon = title %>%
                   str_extract(match.emoji) %>%
                   replace_na("📒"),
               url = "/doc/todo-HMxR5dB0ld") %>%
        relocate(parent, id) %>%        # Parent comes first (to get
                                        # things right in case there
                                        # is only one doc, i.e. when
                                        # running under
                                        # --small-sample)
        FromDataFrameNetwork() %>%
        as.list(mode = "explicit", unname=TRUE,
                nameName = "id", childrenName = "children") %>%
        { .$children }

    rlang::env(tibble = document.tibble,
               tree = document.tree)
}

transform <- function (archive_path, confluence) {
    documentMeta <- transform.documentMetadata(confluence$pages, uuid_salt = archive_path)
    attachments <- transform.attachments(confluence$attachments, documentMeta$tibble, uuid_salt = archive_path)

    meta <- list(
        attachments = attachments$meta,
        collection = list(documentStructure = documentMeta$tree))

    meta.filename <- archive_path %>%
        base::basename() %>%
        str_extract("^(.*?)-[0-9-]*[.]xml[.]zip", group = 1) %>%
        paste0(".json")

    rlang::env(documents = documentMeta,
               attachments = attachments$tibble,
               meta = meta,
               meta.filename = meta.filename)
}

rewrite.attachments <- function (.attachments, zip.from, zip.to) {
    attachments.indexed <- .attachments %>%
        split(.$confluence_zip_path)
    zip.from$files() %>%
        iterate(function(zip.file) {
            attachment <- attachments.indexed[[zip.file$path]]
            if (length(attachment)) {
                as_filename <- attachment$outline.key
                print(as_filename)
                zip.to$add(zip.file, as_filename = as_filename)
            }
        })
}
