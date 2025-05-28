# install.packages("glue")
# install.packages("uuid")
# install.packages("data.tree")

library(data.tree)
library(stringr)

uuidifiers <- function(..., salt) {
    deterministic.uuid <- function (char_v, ...) {
        ## With the `uuid` package, only UUIDs (deterministically) beget
        ## UUIDs for some reason:
        namespace <- "6e28e697-bed4-4e50-b6e5-4efc31b478b9"
        for (salt in rlang::list2(...)) {
            namespace <- uuid::UUIDfromName(namespace, salt)
        }

        uuid::UUIDfromName(namespace, char_v)
    }

    purrr::imap(enquos(...), function (quosure, name) {
        map <- tibble(from = rlang::eval_tidy(quosure)) %>%
            mutate(to = deterministic.uuid(from, name, salt))
        function(from) {
            stopifnot(all(from[! is.na(from)] %in% map$from))
            tibble(from) %>%
                left_join(map, by = join_by(from)) %>%
                pull(to)
        }
    })
}

mutate.list <- function(.data, ...) {
    dots <- enquos(...)
    # Actually we want to “dequos” some of them right away, lest
    # `mutate` think it should create columns named `.keep`:
    mutate.args.common <-
        dots[!str_detect(names(dots), fixed("$"))] %>%
        purrr::map(rlang::eval_tidy)
    purrr::imap(.data, function(df, df.name) {
        prefix <- fixed(paste0(df.name, "$"))
        mutate.scoped <- dots[str_starts(names(dots), prefix)]
        names(mutate.scoped) <- str_replace(names(mutate.scoped), prefix, "")

        do.call(dplyr::mutate, c(list(df), mutate.args.common, mutate.scoped))
    })
}

transform.attachments <- function (attachments) {
    uploads.prefix <- "uploads/restored-from-Confluence"
    attachments %>%
        mutate(key = glue::glue("{ uploads.prefix }/{ id }/{ title }")) %>%
        relocate(key, .after = id)
}

transform.attachments.meta <- function (attachments) {
    attachments %>%
        select(id, documentId, contentType = media.type, name = title, size, key) %>%
        mutate(size = as.character(size),   # Meh.
                                        # XXX
               userId = "7e09f00d-dcbd-4ee5-b4de-395703cfe275") %>%
                                        # /XXX
        split(.$id) %>%
        map(~ as.list(.x))
}

transform.documentStructure <- function (documents) {
    ensure.children.even.if.empty <- function(node) {
        ## Based on empirical evidence in Outline export files, it
        ## seems that `children` should be an empty array, rather
        ## than missing altogether:
        if (! ("children" %in% names(node))) {
            node$children <- list()
        } else {
            node$children <-
                purrr::map(node$children, ensure.children.even.if.empty)
        }
        node
    }

    documents %>%
        transmute(
            id, url, title, color, icon,
            parent = replace_na(parentDocumentId, "__ROOT__")) %>%
        relocate(parent, id) %>%  # Parent comes first (to get things
                                  # right in case there is only one
                                  # doc, i.e. when running under
                                  # --small-sample)
        FromDataFrameNetwork() %>%
        as.list(mode = "explicit", unname=TRUE,
                nameName = "id", childrenName = "children") %>%
        ensure.children.even.if.empty() %>%
        { .$children }
}

#' R wrapper around the Python class of the same name.
DocumentConverter <- function (documents_tibble) {
    source_python("document-converter.py")
    setOldClass("__main__.PythonStruct")  ## Does nothing but squelch a warning
                                          ## in the next stanza, afaict
    setMethod(jsonlite:::asJSON, signature(x="__main__.PythonStruct"),
              function(x, ...) { x$to_json(...) })
    DocumentConverter(   # Not this one — The other one (the Python class)
        documents_tibble = documents_tibble)
}

now.zulu <- strftime(Sys.time(), "%Y-%m-%dT%H:%M:%S.000Z", tz="UTC")

transform.documents <- function (.documents, dc) {
    first.emoji <- function(char_v) {
        match.emoji <- paste0(
            "(?:\\p{Emoji_Presentation}|\\p{Extended_Pictographic})",
            "(?:\\p{Regional_Indicator}|\\p{Emoji_Modifier})*")

        char_v %>% str_extract(match.emoji)
    }

    .documents %>%
        left_join(dc$get_converted_documents() %>% as_tibble(),
                  by = join_by(documentId)) %>%
        transmute(
            id = documentId,
            title = replace_na(title, ""),
            data = outline.document,
                                        # XXX
            createdById = "7e09f00d-dcbd-4ee5-b4de-395703cfe275",
            createdByName = "Nicolas Borboën",
            createdByEmail = "nicolas.borboen@epfl.ch",
                                        # /XXX
            createdAt = now.zulu,
            updatedAt = now.zulu,
            publishedAt = now.zulu,
            fullWidth = FALSE,
            template = FALSE,
            icon = title %>% first.emoji() %>%
                replace_na("📒"),
            color = as.character(NA),
            url = "/doc/todo-TODOabcd",
            urlId = "TODOabcd",
            parentDocumentId)
}

empty.document <- list(type = "doc",
                       content = list(list(type = "paragraph")))

transform.document.meta <- function(documents) {
    documents %>%
        select(-url) %>%
        split(.$id) %>%
        map(function(.x) {
            ret <- as.list(.x)
            ## ret$data is still a list, which would result in
            ## a spurious one-element array in the JSON if we
            ## didn't do this:
            ret$data <- ret$data[[1]]

            if (is.null(ret$data)) {
                ret$data <- empty.document
            }
            ret
        })
}

exclude_unused_attachments <- function(.attachments, dc) {
    ## Most attachments get copied with their parent page, and go
    ## unused as one edits the copy!! Yet they still end up in the Zip
    ## export somehow!
    seen.attachments <-
        dc$list_attachments() %>%
        as_tibble() %>%
        unnest_longer(filename) %>%
        ## As seen in confluence.R, attachments always belong to the
        ## latest version of a page:
        distinct(latest.version, filename)

    ## TODO: this filtering is still sub-optimal! Attachments have
    ## versions too; however, the `ri:filename` attributes in HTML
    ## pages reference them (you gueessed it) by name, without
    ## stipulating a version. I don't know at this point how (or
    ## indeed whether) Confluence picks a particular version for
    ## attachments while rendering a page.
    .attachments %>%
        inner_join(seen.attachments,
                   by = join_by(documentId == latest.version,
                                title == filename))
}

transform <- function (archive_path, confluence) {
    outline <- local({
        u <- uuidifiers("doc_ids" = confluence$pages$page_id,
                        "attch_ids" = confluence$attachments$attachment_id,
                        salt = archive_path)

        list(documents = confluence$pages,
             attachments = confluence$attachments) %>%
            mutate(    # using `mutate.list`, above
                .keep="unused", .before = 1,
                `documents$documentId` = u$doc_ids(page_id),
                `documents$parentDocumentId` = u$doc_ids(parent.Page),
                `documents$latest.version` = u$doc_ids(latest.version),
                `attachments$id` = u$attch_ids(attachment_id),
                `attachments$documentId` = u$doc_ids(containerContent.Page))
    })

    dc <- DocumentConverter(
        outline$documents %>%
        ## NAs would get sent to Python as the string "NA" 🤦
        filter(! is.na(body)))
    transformed.documents <- outline$documents %>%
        transform.documents(dc)

    attachments <-
        outline$attachments %>%
        exclude_unused_attachments(dc) %>%
        transform.attachments()

    collection.blurb <- empty.document

    meta <- list(
        attachments = attachments %>%
            transform.attachments.meta(),
        collection = list(
            name = "Collection restored from Confluence®",
                                        # XXX
            id = "077fde82-7e36-4849-b4e5-a78b9d3feb64",  # TODO: there is a
                                        # glimmer of hope for server-side
                                        # merging here.
            urlId = "82vCJbygJu",
            sort = list(field = "index", direction = "asc"),
            index = "1",
            permission = "read",
            sharing = TRUE,
                                        # /XXX
            createdAt = now.zulu,
            updatedAt = now.zulu,
            deletedAt = as.character(NA),
            archivedAt = as.character(NA),
            color = as.character(NA),
            icon = as.character(NA),
            data = collection.blurb,
            documentStructure = transformed.documents %>%
                transform.documentStructure()),
        documents = transformed.documents %>%
            transform.document.meta())

    meta.filename <- archive_path %>%
        base::basename() %>%
        str_extract("^(.*?)-[0-9-]*[.]xml[.]zip", group = 1) %>%
        paste0(".json")

    list(.tmp = list(outline = outline, dc = dc),
         documents = transformed.documents,
         attachments = attachments,
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
                as_filename <- attachment$key
                print(as_filename)
                zip.to$add(zip.file, as_filename = as_filename)
            }
        })
}
