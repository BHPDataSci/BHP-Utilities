source("load_BHP_utilities.R")
LoadBHPUtilities(path = ".", verbose = FALSE)

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("The RxMap validation script requires `openxlsx`.", call. = FALSE)
}

df_Raw <- tibble::tibble(
  record_id = c(1L, 1L, 2L, 3L),
  visit = c(1L, 2L, 1L, 1L),
  other_name1 = c("Drug A", "Drug B", "Combo", NA_character_),
  other_name2 = c("Drug A", "Unknown", "", NA_character_)
)

df_Input <- CreateRxMapInput(data = df_Raw, WriteFile = FALSE)
stopifnot(
  identical(df_Input$Drug, c("Drug A", "Drug B", "Unknown", "Combo")),
  ncol(df_Input) == 1L
)

missing_column_error <- tryCatch(
  {
    CreateRxMapInput(
      data = df_Raw,
      MedicationColumns = "not_present",
      WriteFile = FALSE
    )
    FALSE
  },
  error = function(e) TRUE
)
stopifnot(missing_column_error)

df_Labelled <- df_Raw
df_Labelled$other_name1 <- haven::labelled(
  df_Labelled$other_name1,
  label = "First medication"
)
df_LabelledInput <- CreateRxMapInput(
  data = df_Labelled,
  WriteFile = FALSE
)
stopifnot(identical(df_LabelledInput$Drug, df_Input$Drug))

df_DrugMapping <- tibble::tibble(
  Drug = c("Drug A", "Drug B", "Combo", "Unknown"),
  llm_guess = c(NA, NA, "COMBINATION PRODUCT", "LIKELY DRUG"),
  rxnorm_guess = c("DRUG A", "DRUG B", "COMBO", NA),
  rxdrug_1 = c("DRUG A", "DRUG B", "INGREDIENT X", NA),
  rxdrug_2 = c(NA, NA, "INGREDIENT Y", NA)
)

df_IngredientMapping <- tibble::tibble(
  Drug = c("Drug A", "Drug B", "Combo", "Unknown"),
  llm_guess = c(NA, NA, NA, "LIKELY INGREDIENT"),
  rxdrug_1 = c("INGREDIENT A", "INGREDIENT B", "INGREDIENT X", NA),
  rxdrug_2 = c(NA, NA, "INGREDIENT Y", NA)
)

df_ATC <- tibble::tibble(
  Drug = c("Drug A", "Drug A", "Drug B", "Combo", "Combo"),
  rxdrug = c(
    "INGREDIENT A", "INGREDIENT A", "INGREDIENT B",
    "INGREDIENT X", "INGREDIENT Y"
  ),
  atc_code = c("A1", "A2", "B1", "C1", "C2"),
  ATC1 = c("CLASS A", "CLASS A", "CLASS B", "CLASS C", "CLASS C"),
  ATC2 = c("A TWO", "A TWO", "B TWO", "C TWO", "C TWO"),
  ATC3 = c("A THREE", "A THREE", "B THREE", "C THREE", "C THREE"),
  ATC4 = c("A FOUR", "A FOUR", "B FOUR", "C FOUR", "C FOUR")
)

fn_RxMap <- tempfile(fileext = ".xlsx")
openxlsx::write.xlsx(
  list(
    "Drug Mapping" = df_DrugMapping,
    "Drug Mapping IN" = df_IngredientMapping,
    "ATC IN" = df_ATC
  ),
  file = fn_RxMap
)

RxMapResult <- ApplyRxMapMappings(
  data = df_Raw,
  RxMapFile = fn_RxMap,
  KeepIDColumns = c("record_id", "visit"),
  ReturnDetails = TRUE
)

df_Features <- RxMapResult$Features
stopifnot(
  nrow(df_Features) == nrow(df_Raw),
  identical(df_Features$.RxMapRowID, seq_len(nrow(df_Raw))),
  anyDuplicated(df_Features$.RxMapRowID) == 0L,
  identical(df_Features$record_id, df_Raw$record_id),
  df_Features$TotalMedicationCount[[1]] == 1L,
  df_Features$TotalMedicationCount[[2]] == 2L,
  df_Features$TotalMedicationCount[[3]] == 1L,
  df_Features$ATC1_CLASS_A[[1]] == 1L,
  df_Features$ATC1_CLASS_C[[3]] == 2L
)

feature_columns <- names(df_Features)[
  stringr::str_detect(
    names(df_Features),
    "^(DrugLevel_|IngredientLevel_|ATC[1-4]_)"
  )
]
stopifnot(
  all(
    vapply(
      df_Features[feature_columns],
      function(x) is.integer(x) && all(!is.na(x)),
      logical(1)
    )
  )
)

df_QC <- SummarizeRxMapQC(RxMapResult)
df_Review <- GetRxMapReviewCases(RxMapResult)
stopifnot(
  "Mappings using an LLM result" %in% df_QC$Metric,
  all(c("Combo", "Unknown") %in% df_Review$Drug)
)

df_CollisionDictionary <- .RxMapFeatureDictionary(
  Concepts = c("Drug-A", "Drug A"),
  Prefix = "DrugLevel_"
)
stopifnot(anyDuplicated(df_CollisionDictionary$FeatureName) == 0L)

invalid_atc_error <- tryCatch(
  {
    ApplyRxMapMappings(
      data = df_Raw,
      RxMapFile = fn_RxMap,
      IncludeATCLevels = 5L
    )
    FALSE
  },
  error = function(e) TRUE
)
stopifnot(invalid_atc_error)

fn_MinimalRxMap <- tempfile(fileext = ".xlsx")
openxlsx::write.xlsx(
  list(
    "Drug Mapping" = tibble::tibble(Drug = "Drug A", rxdrug_1 = "DRUG A"),
    "Drug Mapping IN" = tibble::tibble(
      Drug = "Drug A",
      rxdrug_1 = "INGREDIENT A"
    ),
    "ATC IN" = df_ATC[1, ]
  ),
  file = fn_MinimalRxMap
)
MinimalMappings <- ReadRxMapMappings(fn_MinimalRxMap)
stopifnot(
  is.na(MinimalMappings$DrugMap$llm_guess[[1]]),
  is.na(MinimalMappings$DrugMap$rxnorm_guess[[1]])
)

fn_MissingSheet <- tempfile(fileext = ".xlsx")
openxlsx::write.xlsx(
  list("Drug Mapping" = df_DrugMapping),
  file = fn_MissingSheet
)
missing_sheet_error <- tryCatch(
  {
    ReadRxMapMappings(fn_MissingSheet)
    FALSE
  },
  error = function(e) TRUE
)
stopifnot(missing_sheet_error)

unlink(c(fn_RxMap, fn_MinimalRxMap, fn_MissingSheet))
message("RxMap utility validation passed.")
