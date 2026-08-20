#' Apply RxMap mappings to row-level medication data
#'
#' Creates a merge-ready feature table from medication strings stored across
#' multiple columns. `.RxMapRowID` is the authoritative key, allowing repeated
#' participant identifiers in longitudinal data.
#'
#' @param data A data frame containing medication variables.
#' @param RxMapFile Optional path to the RxMap Excel results workbook. Use
#'   either `RxMapFile` or `Mappings`.
#' @param Mappings Optional mapping object returned by
#'   `ReadRxMapMappings()`. When supplied, this exact reviewed object is used
#'   and `RxMapFile` is not read again.
#' @param MedicationPrefix Prefix identifying medication variables.
#' @param MedicationColumns Optional explicit medication column names.
#' @param KeepIDColumns Optional identifiers retained for readable QA. These
#'   columns do not need to be unique.
#' @param DrugSheet Drug-level mapping sheet.
#' @param IngredientSheet Ingredient-level mapping sheet.
#' @param ATCSheet Ingredient-level ATC sheet.
#' @param ExcludeDrugs Optional original medication strings to exclude.
#' @param IncludeATCLevels ATC hierarchy levels to add.
#' @param ATCCountUnit Whether ATC columns count distinct ingredients or drugs.
#' @param ReturnDetails Whether to return audit tables and dictionaries.
#'
#' @return A keyed feature data frame, or a detailed named list.
ApplyRxMapMappings <- function(
    data,
    RxMapFile = NULL,
    Mappings = NULL,
    MedicationPrefix = "other_name",
    MedicationColumns = NULL,
    KeepIDColumns = NULL,
    DrugSheet = "Drug Mapping",
    IngredientSheet = "Drug Mapping IN",
    ATCSheet = "ATC IN",
    ExcludeDrugs = NULL,
    IncludeATCLevels = 1:4,
    ATCCountUnit = c("ingredient", "drug"),
    ReturnDetails = FALSE
) {

  ATCCountUnit <- match.arg(ATCCountUnit)
  IncludeATCLevels <- sort(unique(IncludeATCLevels))

  if (any(!IncludeATCLevels %in% 1:4)) {
    stop(
      "`IncludeATCLevels` must contain values from 1 through 4.",
      call. = FALSE
    )
  }

  MedicationColumns <- .RxMapMedicationColumns(
    data = data,
    MedicationPrefix = MedicationPrefix,
    MedicationColumns = MedicationColumns
  )

  missing_id_columns <- setdiff(KeepIDColumns, names(data))

  if (length(missing_id_columns) > 0L) {
    stop(
      "The following `KeepIDColumns` are missing from `data`: ",
      paste(missing_id_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(Mappings) && is.null(RxMapFile)) {
    stop(
      "Supply either `Mappings` or `RxMapFile`.",
      call. = FALSE
    )
  }

  if (is.null(Mappings)) {
    Mappings <- ReadRxMapMappings(
      RxMapFile = RxMapFile,
      DrugSheet = DrugSheet,
      IngredientSheet = IngredientSheet,
      ATCSheet = ATCSheet,
      ExcludeDrugs = ExcludeDrugs
    )
  } else {
    required_mapping_tables <- c("DrugMap", "IngredientMap", "ATCMap")
    missing_mapping_tables <- setdiff(
      required_mapping_tables,
      names(Mappings)
    )

    if (length(missing_mapping_tables) > 0L) {
      stop(
        "`Mappings` is missing: ",
        paste(missing_mapping_tables, collapse = ", "),
        call. = FALSE
      )
    }

    if (!is.null(ExcludeDrugs)) {
      Mappings$DrugMap <- Mappings$DrugMap %>%
        dplyr::filter(!(Drug %in% ExcludeDrugs))
      Mappings$IngredientMap <- Mappings$IngredientMap %>%
        dplyr::filter(!(Drug %in% ExcludeDrugs))
      Mappings$ATCMap <- Mappings$ATCMap %>%
        dplyr::filter(!(Drug %in% ExcludeDrugs))
    }
  }

  df_Base <- tibble::tibble(.RxMapRowID = seq_len(nrow(data))) %>%
    dplyr::bind_cols(
      data %>%
        dplyr::select(dplyr::all_of(KeepIDColumns))
    )

  df_MedicationLong <- data %>%
    dplyr::mutate(.RxMapRowID = seq_len(nrow(data))) %>%
    dplyr::select(
      .RxMapRowID,
      dplyr::all_of(MedicationColumns)
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(MedicationColumns),
        as.character
      )
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(MedicationColumns),
      names_to = "MedicationVariable",
      values_to = "Drug"
    ) %>%
    dplyr::filter(!is.na(Drug), trimws(Drug) != "")

  if (!is.null(ExcludeDrugs)) {
    df_MedicationLong <- df_MedicationLong %>%
      dplyr::filter(!(Drug %in% ExcludeDrugs))
  }

  df_MedicationLong <- df_MedicationLong %>%
    dplyr::left_join(Mappings$DrugMap, by = "Drug")

  df_DrugObservations <- df_MedicationLong %>%
    dplyr::filter(!is.na(DrugLevel)) %>%
    dplyr::distinct(.RxMapRowID, DrugLevel)

  df_DrugDictionary <- .RxMapFeatureDictionary(
    Concepts = df_DrugObservations$DrugLevel,
    Prefix = "DrugLevel_"
  )

  df_DrugWide <- df_DrugObservations %>%
    dplyr::left_join(
      df_DrugDictionary,
      by = c("DrugLevel" = "Concept")
    ) %>%
    dplyr::mutate(Value = 1L) %>%
    dplyr::select(.RxMapRowID, FeatureName, Value) %>%
    tidyr::pivot_wider(
      names_from = FeatureName,
      values_from = Value,
      values_fill = 0L,
      values_fn = max
    )

  df_IngredientObservations <- df_MedicationLong %>%
    dplyr::select(
      .RxMapRowID,
      MedicationVariable,
      Drug,
      DrugLevel
    ) %>%
    dplyr::left_join(
      Mappings$IngredientMap,
      by = "Drug",
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(!is.na(IngredientLevel))

  df_IngredientDictionary <- .RxMapFeatureDictionary(
    Concepts = df_IngredientObservations$IngredientLevel,
    Prefix = "IngredientLevel_"
  )

  df_IngredientWide <- df_IngredientObservations %>%
    dplyr::distinct(.RxMapRowID, IngredientLevel) %>%
    dplyr::left_join(
      df_IngredientDictionary,
      by = c("IngredientLevel" = "Concept")
    ) %>%
    dplyr::mutate(Value = 1L) %>%
    dplyr::select(.RxMapRowID, FeatureName, Value) %>%
    tidyr::pivot_wider(
      names_from = FeatureName,
      values_from = Value,
      values_fill = 0L,
      values_fn = max
    )

  df_IngredientATC <- df_IngredientObservations %>%
    dplyr::left_join(
      Mappings$ATCMap,
      by = c("Drug", "IngredientLevel"),
      relationship = "many-to-many"
    )

  ATCWideList <- list()
  ATCDictionaryList <- list()

  for (atc_level_number in IncludeATCLevels) {
    atc_column <- paste0("ATC", atc_level_number)

    df_ATCValues <- df_IngredientATC %>%
      dplyr::filter(
        !is.na(!!rlang::sym(atc_column)),
        trimws(!!rlang::sym(atc_column)) != ""
      )

    if (ATCCountUnit == "ingredient") {
      df_ATCCounts <- df_ATCValues %>%
        dplyr::distinct(
          .RxMapRowID,
          IngredientLevel,
          !!rlang::sym(atc_column)
        ) %>%
        dplyr::count(
          .RxMapRowID,
          !!rlang::sym(atc_column),
          name = "Value"
        )
    } else {
      df_ATCCounts <- df_ATCValues %>%
        dplyr::filter(!is.na(DrugLevel)) %>%
        dplyr::distinct(
          .RxMapRowID,
          DrugLevel,
          !!rlang::sym(atc_column)
        ) %>%
        dplyr::count(
          .RxMapRowID,
          !!rlang::sym(atc_column),
          name = "Value"
        )
    }

    df_ATCDictionary <- .RxMapFeatureDictionary(
      Concepts = df_ATCCounts[[atc_column]],
      Prefix = paste0(atc_column, "_")
    )
    ATCDictionaryList[[atc_column]] <- df_ATCDictionary

    if (nrow(df_ATCCounts) > 0L) {
      ATCWideList[[atc_column]] <- df_ATCCounts %>%
        dplyr::rename(Concept = !!rlang::sym(atc_column)) %>%
        dplyr::left_join(df_ATCDictionary, by = "Concept") %>%
        dplyr::select(.RxMapRowID, FeatureName, Value) %>%
        tidyr::pivot_wider(
          names_from = FeatureName,
          values_from = Value,
          values_fill = 0L,
          values_fn = sum
        )
    }
  }

  df_Features <- df_Base

  for (df_Wide in c(
    list(df_DrugWide, df_IngredientWide),
    ATCWideList
  )) {
    if (ncol(df_Wide) > 1L) {
      df_Features <- df_Features %>%
        dplyr::left_join(df_Wide, by = ".RxMapRowID")
    }
  }

  feature_columns <- names(df_Features)[
    stringr::str_detect(
      names(df_Features),
      "^(DrugLevel_|IngredientLevel_|ATC[1-4]_)"
    )
  ]

  if (length(feature_columns) > 0L) {
    df_Features <- df_Features %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(feature_columns),
          ~ as.integer(tidyr::replace_na(.x, 0L))
        )
      )
  }

  drug_columns <- names(df_Features)[
    startsWith(names(df_Features), "DrugLevel_")
  ]

  if (length(drug_columns) > 0L) {
    df_Features <- df_Features %>%
      dplyr::mutate(
        TotalMedicationCount = as.integer(
          rowSums(
            dplyr::across(dplyr::all_of(drug_columns)),
            na.rm = TRUE
          )
        )
      )
  } else {
    df_Features$TotalMedicationCount <- 0L
  }

  if (
    nrow(df_Features) != nrow(data) ||
      anyDuplicated(df_Features$.RxMapRowID) > 0L ||
      !identical(df_Features$.RxMapRowID, seq_len(nrow(data)))
  ) {
    stop(
      "RxMap feature construction did not preserve one row per source row.",
      call. = FALSE
    )
  }

  if (!ReturnDetails) {
    return(df_Features)
  }

  list(
    Features = df_Features,
    MedicationLong = df_MedicationLong,
    IngredientObservations = df_IngredientObservations,
    IngredientATC = df_IngredientATC,
    Mappings = Mappings,
    Dictionaries = list(
      Drug = df_DrugDictionary,
      Ingredient = df_IngredientDictionary,
      ATC = ATCDictionaryList
    ),
    Settings = list(
      MedicationColumns = MedicationColumns,
      KeepIDColumns = KeepIDColumns,
      IncludeATCLevels = IncludeATCLevels,
      ATCCountUnit = ATCCountUnit
    )
  )
}
