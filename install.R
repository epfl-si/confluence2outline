#' Install dependencies for the Confluence ETL

# install.packages("reticulate")
library(reticulate)

#' Authored by ChatGPT (except for the `message()`). Yes, it worked
#' out-of-the-box on the first attempt.
install_from_comments <- function(path = ".", dry_run = TRUE) {
  files <- list.files(path, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)

  pkg_pattern <- "^\\s*#\\s*install\\.packages\\((['\"])([A-Za-z0-9\\.]+)\\1\\)"

  pkgs <- unlist(lapply(files, function(file) {
    lines <- readLines(file, warn = FALSE)
    matches <- regmatches(lines, regexec(pkg_pattern, lines))
    pkgs <- sapply(matches, function(m) if (length(m) >= 3) m[3] else NA_character_)
    na.omit(pkgs)
  }))

  pkgs <- unique(pkgs)

  if (length(pkgs) == 0) {
    message("No commented-out install.packages() calls found.")
    return(invisible(NULL))
  }

  message("R package dependencies: ", paste(pkgs, collapse = ", "))

  if (!dry_run) {
    install.packages(pkgs)
  }

  invisible(pkgs)
}

install_python_dependencies <- function() {
    py.deps = c("stream-zip", "stream-unzip", "lxml")
    py_install(py.deps)
    py_require(py.deps)
}

install_python_dev_dependencies <- function() {
    # Nothing here for now
}
