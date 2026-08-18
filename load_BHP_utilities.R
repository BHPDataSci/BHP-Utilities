LoadBHPUtilities <- function(
    path = NULL,
    envir = globalenv(),
    recursive = FALSE,
    verbose = interactive(),
    repository = "BHPDataSci/BHP-Utilities",
    ref = "main"
) {

  # source("https://raw.githubusercontent.com/BHPDataSci/BHP-Utilities/main/load_BHP_utilities.R")

  source_type <- "Local"
  temporary_directory <- NULL

  if (is.null(path)) {

    if (dir.exists("R") &&
        (file.exists("README.md") || file.exists(".git"))) {

      path <- getwd()

    } else {

      path <- Sys.getenv("BHP_UTILITIES_PATH", unset = "")

    }

  }

  if (!nzchar(path)) {

    source_type <- "GitHub"
    temporary_directory <- tempfile("BHP-Utilities-")
    dir.create(temporary_directory, recursive = TRUE)
    on.exit(unlink(temporary_directory, recursive = TRUE), add = TRUE)

    fn_Archive <- file.path(temporary_directory, "repository.zip")
    archive_url <- paste0(
      "https://github.com/",
      repository,
      "/archive/",
      ref,
      ".zip"
    )

    tryCatch(
      utils::download.file(
        url = archive_url,
        destfile = fn_Archive,
        mode = "wb",
        quiet = !verbose
      ),
      error = function(e) {
        stop(
          "Unable to download BHP Utilities from GitHub.\n\n",
          "Repository: ", repository, "\n",
          "Reference: ", ref, "\n\n",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )

    utils::unzip(
      zipfile = fn_Archive,
      exdir = temporary_directory
    )

    extracted_directories <- list.dirs(
      path = temporary_directory,
      full.names = TRUE,
      recursive = FALSE
    )

    repository_directories <- extracted_directories[
      dir.exists(file.path(extracted_directories, "R"))
    ]

    if (length(repository_directories) != 1L) {
      stop(
        "The downloaded GitHub archive does not contain one repository-level R directory.",
        call. = FALSE
      )
    }

    path <- repository_directories[[1]]
  }

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  r_directory <- file.path(path, "R")

  if (!dir.exists(r_directory)) {
    stop(
      "No 'R' directory was found in:\n",
      path,
      call. = FALSE
    )
  }

  if (source_type == "Local") {

    version <- tryCatch(
      suppressWarnings(
        system2(
          "git",
          c("-C", shQuote(path), "describe", "--tags", "--abbrev=0"),
          stdout = TRUE,
          stderr = FALSE
        )
      ),
      error = function(e) character()
    )

    if (length(version) == 0) {
      version <- "Development"
    }

    commit <- tryCatch(
      system2(
        "git",
        c("-C", shQuote(path), "rev-parse", "--short", "HEAD"),
        stdout = TRUE,
        stderr = FALSE
      ),
      error = function(e) NA_character_
    )

    modified <- tryCatch({
      status <- system2(
        "git",
        c("-C", shQuote(path), "status", "--porcelain"),
        stdout = TRUE,
        stderr = FALSE
      )
      length(status) > 0
    }, error = function(e) FALSE)

  } else {

    version <- ref
    commit <- if (grepl("^[[:xdigit:]]{7,40}$", ref)) ref else NA_character_
    modified <- FALSE

  }

  r_files <- sort(
    list.files(
      path = r_directory,
      pattern = "\\.[Rr]$",
      full.names = TRUE,
      recursive = recursive
    )
  )

  if (length(r_files) == 0) {
    warning(
      "No R files found in:\n",
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
          "Error while sourcing:\n",
          basename(file),
          "\n\n",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )

  }

  if (verbose) {

    cat(
      "\n",
      "=========================================\n",
      " BHP Utilities\n",
      "=========================================\n",
      " Source  : ", source_type, "\n",
      " Version : ", version, "\n",
      " Commit  : ", commit,
      if (modified) " (modified)" else "",
      "\n",
      " Files   : ", length(r_files), "\n",
      "=========================================\n\n",
      sep = ""
    )

  }

  invisible(
    list(
      version = version,
      commit = commit,
      modified = modified,
      files = basename(r_files),
      n_files = length(r_files),
      path = if (source_type == "Local") path else NA_character_,
      source = source_type,
      repository = if (source_type == "GitHub") repository else NA_character_,
      ref = if (source_type == "GitHub") ref else NA_character_
    )
  )

}
