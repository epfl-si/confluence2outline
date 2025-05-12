#' Call some Python code through https://rstudio.github.io/reticulate

setup_pyenv <- function() {
    python.dir <- "./python-venv"
    if (! virtualenv_exists(python.dir)) {
        virtualenv_create(python.dir)
    }

    use_virtualenv(python.dir, required = TRUE)
}
