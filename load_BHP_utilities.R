LoadBHPUtilities <- function(
    path = NULL,
    envir = globalenv(),
    recursive = FALSE,
    verbose = interactive()
) {

  # Locate the repository
  if (is.null(path)) {
    path <- Sys.getenv("BHP_UTILITIES_PATH", unset = "")
  }

  if (!nzchar(path)) {
    stop(
      "BHP Utilities could not be located.\n",
      "Provide `path` directly or set the BHP_UTILITIES_PATH environment variable.",
      call. = FALSE
    )
  }

  path <- normalizePath(path, mustWork = FALSE)

  r_directory <- file.path(path, "R")

  if (!dir.exists(r_directory)) {
    stop(
      "The R directory does not exist:\n",
      r_directory,
      call. = FALSE
    )
  }

  ## -----------------------------
  ## Determine Git version
  ## -----------------------------

  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(path)

  version <- tryCatch(
    system(
      "git describe --tags --abbrev=0",
      intern = TRUE,
      ignore.stderr = TRUE
    ),
    error = function(e) "Development"
  )

  if (length(version) == 0 || identical(version, "")) {
    version <- "Development"
  }

  commit <- tryCatch(
    system(
      "git rev-parse --short HEAD",
      intern = TRUE,
      ignore.stderr = TRUE
    ),
    error = function(e) NA_character_
  )

  dirty <- tryCatch({
    status <- system(
      "git status --porcelain",
      intern = TRUE,
      ignore.stderr = TRUE
    )
    length(status) > 0
  }, error = function(e) FALSE)

  ## -----------------------------
  ## Load files
  ## -----------------------------

  r_files <- sort(
    list.files(
      path = r_directory,
      pattern = "\\.R$",
      full.names = TRUE,
      recursive = recursive,
      ignore.case = TRUE
    )
  )

  if (length(r_files) == 0L) {
    warning(
      "No R files were found in:\n",
      r_directory,
      call. = FALSE
    )

    return(invisible(NULL))
  }

  for (file in r_files) {

    if (verbose) {
      message("Loading: ", basename(file))
    }

    tryCatch(
      sys.source(
        file,
        envir = envir,
        chdir = TRUE
      ),
      error = function(e) {
        stop(
          "Unable to load ",
          basename(file),
          "\n",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }

  ## -----------------------------
  ## Startup message
  ## -----------------------------

  if (verbose) {

    message("\n",
            "==============================\n",
            "BHP Utilities\n",
            "Version : ", version, "\n",
            "Commit  : ", commit,
            if (dirty) " (modified)" else "",
            "\n",
            "Files   : ", length(r_files), "\n",
            "==============================")
  }

  invisible(
    list(
      version = version,
      commit = commit,
      modified = dirty,
      files = basename(r_files),
      path = path
    )
  )

}