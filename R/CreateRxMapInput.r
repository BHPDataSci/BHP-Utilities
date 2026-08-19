#' Create an RxMap input file from wide medication columns
#'
#' Converts free-text medication variables stored across multiple columns into
#' the one-column format required by RxMap. Medication text is preserved
#' exactly; only missing values, blank values, and duplicate strings are
#' removed.
#'
#' @param data A data frame containing medication variables.
#' @param OutputFile Path to the CSV file written for RxMap.
#' @param MedicationPrefix Prefix identifying medication variables.
#' @param MedicationColumns Optional explicit medication column names. These
#'   take precedence over `MedicationPrefix`.
#' @param WriteFile Whether to write the CSV to disk.
#'
#' @return A data frame with one character column named `Drug`.
#'
#' @examples
#' \dontrun{
#' df_RxMapInput <- CreateRxMapInput(
#'   data = df_Raw,
#'   MedicationPrefix = "other_name"
#' )
#' }
CreateRxMapInput <- function(
    data,
    OutputFile = "RxMap_Input.csv",
    MedicationPrefix = "other_name",
    MedicationColumns = NULL,
    WriteFile = TRUE
) {

  MedicationColumns <- .RxMapMedicationColumns(
    data = data,
    MedicationPrefix = MedicationPrefix,
    MedicationColumns = MedicationColumns
  )

  df_Output <- data %>%
    dplyr::select(dplyr::all_of(MedicationColumns)) %>%
    dplyr::mutate(
      dplyr::across(dplyr::everything(), as.character)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "SourceColumn",
      values_to = "Drug"
    ) %>%
    dplyr::filter(
      !is.na(Drug),
      trimws(Drug) != ""
    ) %>%
    dplyr::distinct(Drug)

  if (WriteFile) {
    readr::write_csv(df_Output, OutputFile, na = "")
    message(
      nrow(df_Output),
      " unique medication names written to ",
      OutputFile
    )
  }

  df_Output
}
