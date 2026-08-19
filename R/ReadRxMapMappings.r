#' Read and interpret an RxMap results workbook
#'
#' Reads the drug, ingredient, and ingredient-level ATC sheets produced by
#' RxMap and creates standardized mapping dictionaries.
#'
#' @param RxMapFile Path to the Excel workbook exported by RxMap.
#' @param DrugSheet Drug-level mapping sheet.
#' @param IngredientSheet Ingredient-level mapping sheet.
#' @param ATCSheet Ingredient-level ATC sheet.
#' @param ExcludeDrugs Optional original medication strings to exclude.
#'
#' @return A list containing processed mapping dictionaries and raw sheets.
ReadRxMapMappings <- function(
    RxMapFile,
    DrugSheet = "Drug Mapping",
    IngredientSheet = "Drug Mapping IN",
    ATCSheet = "ATC IN",
    ExcludeDrugs = NULL
) {

  if (!file.exists(RxMapFile)) {
    stop("`RxMapFile` does not exist: ", RxMapFile, call. = FALSE)
  }

  available_sheets <- readxl::excel_sheets(RxMapFile)
  required_sheets <- c(DrugSheet, IngredientSheet, ATCSheet)
  missing_sheets <- setdiff(required_sheets, available_sheets)

  if (length(missing_sheets) > 0L) {
    stop(
      "The following required RxMap sheets were not found: ",
      paste(missing_sheets, collapse = ", "),
      call. = FALSE
    )
  }

  df_DrugRaw <- readxl::read_excel(RxMapFile, sheet = DrugSheet) %>%
    .RxMapStandardizeSheet()
  df_IngredientRaw <- readxl::read_excel(
    RxMapFile,
    sheet = IngredientSheet
  ) %>%
    .RxMapStandardizeSheet()
  df_ATCRaw <- readxl::read_excel(RxMapFile, sheet = ATCSheet) %>%
    .RxMapStandardizeSheet()

  for (sheet_name in c("Drug Mapping", "Drug Mapping IN")) {
    df_Sheet <- if (sheet_name == "Drug Mapping") {
      df_DrugRaw
    } else {
      df_IngredientRaw
    }

    if (!"Drug" %in% names(df_Sheet)) {
      stop(
        "The ", sheet_name, " sheet does not contain `Drug`.",
        call. = FALSE
      )
    }

    duplicated_drugs <- unique(
      df_Sheet$Drug[duplicated(df_Sheet$Drug) & !is.na(df_Sheet$Drug)]
    )

    if (length(duplicated_drugs) > 0L) {
      stop(
        "The ", sheet_name,
        " sheet contains duplicate submitted Drug values: ",
        paste(utils::head(duplicated_drugs, 10L), collapse = ", "),
        call. = FALSE
      )
    }
  }

  if (!"llm_guess" %in% names(df_DrugRaw)) {
    df_DrugRaw$llm_guess <- NA_character_
  }

  if (!"rxnorm_guess" %in% names(df_DrugRaw)) {
    df_DrugRaw$rxnorm_guess <- NA_character_
  }

  if (!"llm_guess" %in% names(df_IngredientRaw)) {
    df_IngredientRaw$llm_guess <- NA_character_
  }

  df_DrugProcessed <- .RxMapFinalValues(df_DrugRaw)

  df_DrugMap <- df_DrugProcessed %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      DrugLevel = if (.NFinal == 1L) {
        as.character(.FinalValues[[1]][1])
      } else if (.NFinal > 1L && !is.na(llm_guess)) {
        as.character(llm_guess)
      } else if (.NFinal > 1L) {
        paste(sort(.FinalValues[[1]]), collapse = " / ")
      } else if (!is.na(llm_guess)) {
        as.character(llm_guess)
      } else {
        NA_character_
      },
      MappingSource = if (.NFinal == 1L) {
        "RxMap_Final"
      } else if (.NFinal > 1L && !is.na(llm_guess)) {
        "LLM_Canonical_MultiMapping"
      } else if (.NFinal > 1L) {
        "RxMap_Combined"
      } else if (!is.na(llm_guess)) {
        "LLM_Fallback"
      } else {
        "Unmapped"
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Drug,
      DrugLevel,
      MappingSource,
      NumberFinalMappings = .NFinal,
      llm_guess,
      rxnorm_guess
    )

  df_IngredientProcessed <- .RxMapFinalValues(df_IngredientRaw)

  df_IngredientFinal <- df_IngredientProcessed %>%
    dplyr::select(Drug, .FinalValues) %>%
    tidyr::unnest_longer(
      .FinalValues,
      values_to = "IngredientLevel"
    ) %>%
    dplyr::filter(!is.na(IngredientLevel)) %>%
    dplyr::transmute(
      Drug,
      IngredientLevel = as.character(IngredientLevel),
      MappingSource = "RxMap_Final"
    )

  df_IngredientFallback <- df_IngredientProcessed %>%
    dplyr::filter(.NFinal == 0L, !is.na(llm_guess)) %>%
    dplyr::transmute(
      Drug,
      IngredientLevel = as.character(llm_guess),
      MappingSource = "LLM_Fallback"
    )

  df_IngredientMap <- dplyr::bind_rows(
    df_IngredientFinal,
    df_IngredientFallback
  ) %>%
    dplyr::distinct(Drug, IngredientLevel, .keep_all = TRUE)

  required_atc_columns <- c(
    "Drug", "rxdrug", "ATC1", "ATC2", "ATC3", "ATC4"
  )
  missing_atc_columns <- setdiff(required_atc_columns, names(df_ATCRaw))

  if (length(missing_atc_columns) > 0L) {
    stop(
      "The ATC IN sheet is missing: ",
      paste(missing_atc_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (!"atc_code" %in% names(df_ATCRaw)) {
    df_ATCRaw$atc_code <- NA_character_
  }

  df_ATCMap <- df_ATCRaw %>%
    dplyr::transmute(
      Drug,
      IngredientLevel = rxdrug,
      atc_code,
      ATC1,
      ATC2,
      ATC3,
      ATC4
    ) %>%
    dplyr::filter(!is.na(Drug), !is.na(IngredientLevel)) %>%
    dplyr::distinct()

  if (!is.null(ExcludeDrugs)) {
    df_DrugMap <- df_DrugMap %>%
      dplyr::filter(!(Drug %in% ExcludeDrugs))
    df_IngredientMap <- df_IngredientMap %>%
      dplyr::filter(!(Drug %in% ExcludeDrugs))
    df_ATCMap <- df_ATCMap %>%
      dplyr::filter(!(Drug %in% ExcludeDrugs))
  }

  list(
    DrugMap = df_DrugMap,
    IngredientMap = df_IngredientMap,
    ATCMap = df_ATCMap,
    DrugRaw = df_DrugRaw,
    IngredientRaw = df_IngredientRaw,
    ATCRaw = df_ATCRaw
  )
}
