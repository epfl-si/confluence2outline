# install.packages("glue")
# install.packages("uuid")

# confluence.R provides attachments

outline.attachments <- {
    attachment.salt <- "6e28e697-bed4-4e50-b6e5-4efc31b478b9"
    uploads.prefix <- "uploads/restored-from-Confluence"
    attachments %>%
        mutate(attachment_uuid = uuid::UUIDfromName(attachment.salt, confluence_zip_path),
               key = glue::glue("{ uploads.prefix }/{ attachment_uuid }/{ title }")) %>%
        relocate(attachment_uuid, key) %>%
        select(-attachment_id)
}

outline.attachments.meta <-
    outline.attachments %>%
    transmute(id = attachment_uuid, documentId = "TODO",
              key,
              contentType = media.type, size, name = title) %>%
    split(.$id) %>%
    map(~ as.list(.x))
