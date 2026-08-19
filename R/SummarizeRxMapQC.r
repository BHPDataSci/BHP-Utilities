#' Summarize RxMap medication mapping quality
#'
#' @param Mappings A result from `ReadRxMapMappings()`, or a detailed result
#'   from `ApplyRxMapMappings()`.
#'
#' @return A tidy data frame of QC metrics, counts, and percentages.
SummarizeRxMapQC <- function(Mappings) {

  Details <- NULL

  if (all(c("Mappings", "Features") %in% names(Mappings))) {
    Details <- Mappings
    Mappings <- Details$Mappings
  }

  if (is.null(Mappings$DrugMap)) {
    stop(
      "`Mappings` must be an RxMap mapping or detailed application result.",
      call. = FALSE
    )
  }

  df_DrugMap <- Mappings$DrugMap
  total <- nrow(df_DrugMap)

  AddMetric <- function(Metric, N, Denominator = total) {
    tibble::tibble(
      Metric = Metric,
      N = as.integer(N),
      Percent = if (Denominator > 0L) {
        round(100 * N / Denominator, 1)
      } else {
        NA_real_
      }
    )
  }

  df_Summary <- AddMetric(
    "Total submitted medication names",
    total,
    total
  )

  if (total > 0L) {
    df_BySource <- df_DrugMap %>%
      dplyr::count(MappingSource, name = "N") %>%
      dplyr::mutate(
        Metric = paste0("Mapping source: ", MappingSource),
        Percent = round(100 * N / total, 1)
      ) %>%
      dplyr::select(Metric, N, Percent)

    df_Summary <- dplyr::bind_rows(
      df_Summary,
      df_BySource,
      AddMetric(
        "Mappings using an LLM result",
        sum(
          df_DrugMap$MappingSource %in% c(
            "LLM_Fallback",
            "LLM_Canonical_MultiMapping"
          )
        )
      ),
      AddMetric(
        "Mappings with multiple finalized concepts",
        sum(df_DrugMap$NumberFinalMappings > 1L, na.rm = TRUE)
      ),
      AddMetric(
        "Unmapped medication names",
        sum(df_DrugMap$MappingSource == "Unmapped", na.rm = TRUE)
      )
    )
  }

  if (!is.null(Details)) {
    feature_names <- names(Details$Features)
    n_occurrences <- nrow(Details$MedicationLong)

    df_Summary <- dplyr::bind_rows(
      df_Summary,
      AddMetric(
        "Medication occurrences in source rows",
        n_occurrences,
        n_occurrences
      ),
      AddMetric(
        "Drug-level binary features created",
        sum(startsWith(feature_names, "DrugLevel_")),
        0L
      ),
      AddMetric(
        "Ingredient-level binary features created",
        sum(startsWith(feature_names, "IngredientLevel_")),
        0L
      ),
      AddMetric(
        "ATC count features created",
        sum(stringr::str_detect(feature_names, "^ATC[1-4]_")),
        0L
      )
    )
  }

  df_Summary
}
