#' Call some Python code through https://rstudio.github.io/reticulate

setup_pyenv <- function() {
    python.dir <- "./python-venv"

    install.required <- ! virtualenv_exists(python.dir)

    if (install.required) {
        virtualenv_create(python.dir)
    }

    use_virtualenv(python.dir, required = TRUE)

    list(changed = install.required)
}
