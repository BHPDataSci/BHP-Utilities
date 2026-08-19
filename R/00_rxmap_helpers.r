`%>%` <- magrittr::`%>%`


.RxMapMissingToNA <- function(x) {

  x <- as.character(x)
  missing_value <- is.na(x) |
    trimws(x) == "" |
    tolower(trimws(x)) == "na"

  x[missing_value] <- NA_character_
  x
}


.RxMapMedicationColumns <- function(
    data,
    MedicationPrefix,
    MedicationColumns
) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (is.null(MedicationColumns)) {
    MedicationColumns <- names(data)[
      startsWith(names(data), MedicationPrefix)
    ]
  }

  if (length(MedicationColumns) == 0L) {
    stop("No medication columns were found.", call. = FALSE)
  }

  missing_columns <- setdiff(MedicationColumns, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      "The following medication columns are missing from `data`: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  unique(MedicationColumns)
}


.RxMapFeatureDictionary <- function(Concepts, Prefix) {

  Concepts <- sort(
    unique(
      Concepts[
        !is.na(Concepts) &
          trimws(Concepts) != ""
      ]
    )
  )

  if (length(Concepts) == 0L) {
    return(
      tibble::tibble(
        Concept = character(),
        FeatureName = character()
      )
    )
  }

  clean_names <- Concepts %>%
    stringr::str_to_upper() %>%
    stringr::str_replace_all("[^A-Z0-9]+", "_") %>%
    stringr::str_replace_all("^_+|_+$", "")

  clean_names[clean_names == ""] <- "UNNAMED"
  clean_names <- make.unique(clean_names, sep = "_")

  tibble::tibble(
    Concept = Concepts,
    FeatureName = paste0(Prefix, clean_names)
  )
}


.RxMapFinalValues <- function(data, Prefix = "rxdrug_") {

  final_columns <- grep(
    paste0("^", Prefix, "[0-9]+$"),
    names(data),
    value = TRUE
  )

  if (length(final_columns) == 0L) {
    stop(
      "No finalized `", Prefix, "*` columns were found in the RxMap sheet.",
      call. = FALSE
    )
  }

  data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(final_columns),
        .RxMapMissingToNA
      )
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      .FinalValues = list(
        unique(
          stats::na.omit(
            dplyr::c_across(dplyr::all_of(final_columns))
          )
        )
      ),
      .NFinal = length(.FinalValues)
    ) %>%
    dplyr::ungroup()
}


.RxMapStandardizeSheet <- function(data) {

  data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character),
        .RxMapMissingToNA
      )
    )
}
