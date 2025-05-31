# install.packages("glue")
# install.packages("uuid")

deterministic.uuid <- function (char_v, ...) {
    ## With the `uuid` package, only UUIDs (deterministically) beget
    ## UUIDs for some reason:
    namespace <- "6e28e697-bed4-4e50-b6e5-4efc31b478b9"
    for (salt in rlang::list2(...)) {
        namespace <- uuid::UUIDfromName(namespace, salt)
    }

    uuid::UUIDfromName(namespace, char_v)
}

transform.attachments <- function (.confluence_attachments, uuid_salt) {
    uploads.prefix <- "uploads/restored-from-Confluence"
    .confluence_attachments %>%
        mutate(attachment_uuid = deterministic.uuid(attachment_id, "attachment_id", uuid_salt),
               key = glue::glue("{ uploads.prefix }/{ attachment_uuid }/{ title }")) %>%
        relocate(attachment_uuid, key) %>%
        select(-attachment_id)
}

transform.attachments.meta <- function (attachments) {
    attachments %>%
        transmute(id = attachment_uuid, documentId = "TODO",
                  key,
                  contentType = media.type, size, name = title) %>%
        split(.$id) %>%
        map(~ as.list(.x))
}

transform <- function (archive_path, confluence) {
    attachments <- transform.attachments(confluence$attachments, uuid_salt = archive_path)

    meta.filename <- archive_path %>%
        base::basename() %>%
        str_extract("^(.*?)-[0-9-]*[.]xml[.]zip", group = 1) %>%
        paste0(".json")

    list(attachments = attachments,
         meta = list(attachments = attachments %>%
                         transform.attachments.meta()),
         meta.filename = meta.filename)
}

rewrite.attachments <- function (.attachments, zip.from, zip.to) {
    attachments.indexed <- .attachments %>%
        split(.$confluence_zip_path)
    zip.from$files() %>%
        iterate(function(zip.file) {
            attachment <- attachments.indexed[[zip.file$path]]
            if (length(attachment)) {
                as_filename <- attachment$key
                print(as_filename)
                zip.to$add(zip.file, as_filename = as_filename)
            }
        })
}
