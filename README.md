# BHP Utilities

Source the loader and load the current utilities with:

```r
source(
  "https://raw.githubusercontent.com/BHPDataSci/BHP-Utilities/main/load_BHP_utilities.R"
)
LoadBHPUtilities()
```

## RxMap medication workflow

Create one deduplicated medication list from unordered wide medication
columns. The original text is deliberately left unchanged for RxMap.

```r
df_RxMapInput <- CreateRxMapInput(
  data = df_Raw,
  MedicationPrefix = "other_name",
  OutputFile = "RxMap_Input.csv"
)
```

Run that CSV through RxMap outside R. After downloading the RxMap workbook,
read the mappings and inspect the automated QC and review tables.

```r
Mappings <- ReadRxMapMappings(
  RxMapFile = "automatic_mappings.xlsx"
)

SummarizeRxMapQC(Mappings)
GetRxMapReviewCases(Mappings)
```

Create row-level drug, ingredient, and ATC features. `.RxMapRowID` is the
merge key and remains unique even when `record_id` repeats across visits.

```r
RxMapResult <- ApplyRxMapMappings(
  data = df_Raw,
  RxMapFile = "automatic_mappings.xlsx",
  KeepIDColumns = c("record_id", "redcap_event_name"),
  ReturnDetails = TRUE
)

df_RxMapFeatures <- RxMapResult$Features
SummarizeRxMapQC(RxMapResult)

df_Merged <- df_Raw %>%
  dplyr::mutate(.RxMapRowID = dplyr::row_number()) %>%
  dplyr::left_join(
    df_RxMapFeatures %>%
      dplyr::select(-dplyr::any_of(c("record_id", "redcap_event_name"))),
    by = ".RxMapRowID"
  )
```

The `DrugLevel_*` and `IngredientLevel_*` columns are binary `0/1`
indicators. `ATC1_*` through `ATC4_*` count distinct mapped ingredients by
default, and `TotalMedicationCount` counts distinct drug-level concepts.
LLM-derived mappings are included but remain explicitly identified in the QC
and review outputs.
