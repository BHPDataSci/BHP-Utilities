#' Identify RxMap medication mappings requiring manual review
#'
#' @param Mappings A result from `ReadRxMapMappings()`, or a detailed result
#'   from `ApplyRxMapMappings()`.
#' @param IncludeMultiMapping Include mappings with multiple finalized drugs.
#' @param IncludeLLMFallback Include mappings that use an LLM result.
#' @param IncludeUnmapped Include completely unmapped medication strings.
#'
#' @return A drug-level review table.
GetRxMapReviewCases <- function(
    Mappings,
    IncludeMultiMapping = TRUE,
    IncludeLLMFallback = TRUE,
    IncludeUnmapped = TRUE
) {

  if (all(c("Mappings", "Features") %in% names(Mappings))) {
    Mappings <- Mappings$Mappings
  }

  if (is.null(Mappings$DrugMap)) {
    stop(
      "`Mappings` must be an RxMap mapping or detailed application result.",
      call. = FALSE
    )
  }

  df_Review <- Mappings$DrugMap
  keep <- rep(FALSE, nrow(df_Review))

  if (IncludeMultiMapping) {
    keep <- keep |
      (!is.na(df_Review$NumberFinalMappings) &
        df_Review$NumberFinalMappings > 1L)
  }

  if (IncludeLLMFallback) {
    keep <- keep |
      df_Review$MappingSource %in% c(
        "LLM_Fallback",
        "LLM_Canonical_MultiMapping"
      )
  }

  if (IncludeUnmapped) {
    keep <- keep | df_Review$MappingSource == "Unmapped"
  }

  df_Review %>%
    dplyr::filter(keep) %>%
    dplyr::arrange(MappingSource, Drug)
}
