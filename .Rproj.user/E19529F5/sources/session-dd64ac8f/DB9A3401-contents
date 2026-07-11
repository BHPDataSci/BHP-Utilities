#' Score questionnaire-derived STRAW+10 reproductive aging variables
#'
#' @description
#' `score_straw10_questionnaire()` derives reproductive aging stage and
#' menopause type from a questionnaire based on the Stages of Reproductive
#' Aging Workshop +10, or STRAW+10, framework.
#'
#' STRAW+10 describes reproductive aging using menstrual-cycle characteristics
#' and time relative to the final menstrual period. Key menstrual criteria
#' include:
#'
#' * Repeated changes of approximately seven days in cycle timing for the early
#'   menopausal transition.
#' * At least one interval of amenorrhea lasting 60 days or longer for the late
#'   menopausal transition.
#' * Time since the final menstrual period for postmenopausal staging.
#'
#' This function implements the questionnaire logic used in the Johns Hopkins
#' Blood Brain Barrier REDCap study and adapted from the supplied Human
#' Connectome Project Aging scoring workflow.
#'
#' The function separates three related but distinct concepts:
#'
#' 1. `stage_detailed`: The reproductive stage suggested by menstrual timing.
#' 2. `menopause_type`: Whether amenorrhea was reported as natural, surgical,
#'    medication-related, treatment-related, or otherwise non-natural.
#' 3. `stage_valid_yn`: Whether the natural STRAW stage can be used without
#'    manual adjudication.
#'
#' @details
#' The original HCP implementation combined reproductive stage, the reason
#' periods stopped, and data-quality concerns into a single decimal code.
#' This function retains a comparable legacy code but also creates separate,
#' interpretable variables.
#'
#' Reproductive stage is assigned using the following hierarchy:
#'
#' 1. Postmenopausal timing stage, when time since the last menstrual period
#'    is available.
#' 2. Late menopausal transition, when a 60-day interval without menstruation
#'    is reported.
#' 3. Early menopausal transition, when cycle timing changed by at least one
#'    week on at least two occasions.
#' 4. Late reproductive stage, when bleeding duration or amount changed.
#' 5. Reproductive stage, when neither bleeding duration nor amount changed.
#'
#' If responses support more than one stage, the most advanced stage is
#' returned and the row is flagged for manual review.
#'
#' @section Surgical menopause:
#'
#' For the default Blood Brain Barrier coding, a response of `2` to
#' `straw1b` or `straw2c` means:
#'
#' `"I have had a hysterectomy or endometrial ablation."`
#'
#' The function assigns these records to:
#'
#' `"Surgical menopause reported"`
#'
#' This is an operational analysis category, not confirmation of bilateral
#' oophorectomy. Hysterectomy without removal of both ovaries and endometrial
#' ablation do not necessarily cause ovarian menopause. Therefore:
#'
#' * `surgical_menopause_reported_yn` is set to 1.
#' * `surgical_menopause_confirmed_yn` remains `NA` unless a separate
#'   confirmation variable is supplied.
#' * The record is flagged for manual review by default.
#' * The natural STRAW timing stage is considered invalid unless surgical
#'   menopause is independently confirmed.
#'
#' A future dataset can provide a separate variable documenting bilateral
#' oophorectomy or bilateral salpingo-oophorectomy through the
#' `surgical_menopause_confirmed` column mapping.
#'
#' @param data A data frame containing questionnaire responses. The function
#'   preserves all original rows and columns.
#'
#' @param columns A named character vector mapping questionnaire concepts to
#'   columns in `data`. Default names match the Blood Brain Barrier REDCap
#'   export.
#'
#'   Recognized mapping names are:
#'
#'   * `period_past_year`
#'   * `last_period`
#'   * `stopped_reason`
#'   * `stopped_reason_other`
#'   * `skipped_60_days`
#'   * `skipped_due_to_age`
#'   * `skipped_reason`
#'   * `skipped_reason_other`
#'   * `repeated_cycle_change`
#'   * `bleeding_duration_change`
#'   * `bleeding_amount_change`
#'   * `hormonal_medication`
#'   * `form_complete`
#'   * `surgical_menopause_confirmed`
#'
#' @param prefix Character prefix for the derived variables. Defaults to
#'   `"straw_"`.
#'
#' @param require_complete_form Logical. When `TRUE`, incomplete or unverified
#'   REDCap forms require manual review. Defaults to `FALSE`.
#'
#' @param hormonal_medication_invalid Logical. When `TRUE`, current hormonal
#'   medication use invalidates automatic natural menstrual staging pending
#'   review. Defaults to `TRUE`.
#'
#' @param surgical_response_is_category Logical. When `TRUE`, the combined
#'   hysterectomy/endometrial-ablation response is assigned to the
#'   `"Surgical menopause reported"` category. Defaults to `TRUE`.
#'
#' @param require_surgical_confirmation Logical. When `TRUE`, a surgical
#'   menopause response remains unconfirmed and requires review unless the
#'   optional confirmation variable equals 1. Defaults to `TRUE`.
#'
#' @param legacy_missing_as_unknown Logical. When `TRUE`, a missing response
#'   for why periods stopped is treated like the explicit `"don't know"`
#'   response when creating the HCP-style legacy code. The recommended default
#'   is `FALSE`.
#'
#' @param add_variable_labels Logical. When `TRUE` and `sjlabelled` is
#'   installed, labels are added to derived variables.
#'
#' @return
#' The original data frame with derived variables appended.
#'
#' Important outputs include:
#'
#' * `straw_stage_detailed`
#' * `straw_stage_6level`
#' * `straw_stage_3level`
#' * `straw_menopause_type`
#' * `straw_surgical_menopause_reported_yn`
#' * `straw_surgical_menopause_confirmed_yn`
#' * `straw_natural_menopause_yn`
#' * `straw_stage_valid_yn`
#' * `straw_manual_review_yn`
#' * `straw_review_reason`
#'
#' @section Default response coding:
#'
#' `straw1a`, time since last period:
#'
#' * 1 = Less than two years
#' * 2 = Two to three years
#' * 3 = Three to six years
#' * 4 = More than six years
#'
#' `straw1b`, why periods stopped:
#'
#' * 1 = Natural menopause
#' * 2 = Hysterectomy or endometrial ablation
#' * 3 = Pregnancy or breastfeeding
#' * 4 = Medication, injection, or hormonal IUD
#' * 5 = Chemotherapy or cancer treatment
#' * 6 = Major weight loss, illness, or stress
#' * 7 = Does not know
#' * 8 = Other
#'
#' `straw2c`, why periods were skipped:
#'
#' * 1 = Pregnancy or breastfeeding
#' * 2 = Hysterectomy or endometrial ablation
#' * 3 = Medication or birth-control injection
#' * 4 = Chemotherapy
#' * 5 = Does not know
#' * 6 = Other
#'
#' Most yes/no questions use 1 = yes and 2 = no.
#'
#' For the Blood Brain Barrier REDCap export, `straw5` uses:
#'
#' * 1 = No hormonal medication
#' * 2 = Yes, currently using birth control
#'
#' @section Interpretation:
#'
#' The stage and menopause-type variables should be interpreted together.
#'
#' Example:
#'
#' ```
#' straw_stage_detailed = "Postmenopause +1b"
#' straw_menopause_type = "Surgical menopause reported"
#' straw_surgical_menopause_confirmed_yn = NA
#' straw_stage_valid_yn = 0
#' straw_manual_review_yn = 1
#' ```
#'
#' This means the participant reported that their last period occurred two to
#' three years ago and selected hysterectomy or endometrial ablation as the
#' reason periods stopped. The record belongs in the surgical category for
#' analysis, but the questionnaire does not confirm bilateral oophorectomy.
#'
#' @section Limitations:
#'
#' This is a questionnaire-derived staging algorithm. It does not independently
#' diagnose menopause and does not infer ovarian removal from hysterectomy or
#' endometrial ablation.
#'
#' The default postmenopausal intervals reproduce the supplied HCP
#' questionnaire categories, but they do not exactly reproduce every formal
#' STRAW+10 postmenopausal substage.
#'
#' @references
#' Harlow SD, Gass M, Hall JE, et al. Executive summary of the Stages of
#' Reproductive Aging Workshop +10: addressing the unfinished agenda of
#' staging reproductive aging. Journal of Clinical Endocrinology and
#' Metabolism. 2012;97(4):1159-1168.
#' doi:10.1210/jc.2011-3362.
#'
#' @examples
#' \dontrun{
#' scored_data <- score_straw10_questionnaire(redcap_data)
#'
#' scored_data |>
#'   dplyr::count(
#'     straw_stage_3level,
#'     straw_menopause_type,
#'     straw_stage_valid_yn
#'   )
#'
#' surgical_records <- scored_data |>
#'   dplyr::filter(
#'     straw_menopause_type == "Surgical menopause reported"
#'   )
#' }
#'
#' @export
score_straw10_questionnaire <- function(
    data,
    columns = c(
      period_past_year = "straw1",
      last_period = "straw1a",
      stopped_reason = "straw1b",
      stopped_reason_other = "straw1b8",
      skipped_60_days = "straw2a",
      skipped_due_to_age = "straw2b",
      skipped_reason = "straw2c",
      skipped_reason_other = "straw2c6",
      repeated_cycle_change = "straw3b",
      bleeding_duration_change = "straw4a",
      bleeding_amount_change = "straw4b",
      hormonal_medication = "straw5",
      form_complete = "straw_10_complete",
      surgical_menopause_confirmed = NA_character_
    ),
    prefix = "straw_",
    require_complete_form = FALSE,
    hormonal_medication_invalid = TRUE,
    surgical_response_is_category = TRUE,
    require_surgical_confirmation = TRUE,
    legacy_missing_as_unknown = FALSE,
    add_variable_labels = TRUE
) {

  # ---------------------------------------------------------------------------
  # Input validation
  # ---------------------------------------------------------------------------

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!is.character(columns) || is.null(names(columns))) {
    stop(
      "`columns` must be a named character vector.",
      call. = FALSE
    )
  }

  if (
    !is.character(prefix) ||
    length(prefix) != 1L ||
    is.na(prefix)
  ) {
    stop(
      "`prefix` must be one non-missing character string.",
      call. = FALSE
    )
  }

  expected_column_keys <- c(
    "period_past_year",
    "last_period",
    "stopped_reason",
    "stopped_reason_other",
    "skipped_60_days",
    "skipped_due_to_age",
    "skipped_reason",
    "skipped_reason_other",
    "repeated_cycle_change",
    "bleeding_duration_change",
    "bleeding_amount_change",
    "hormonal_medication",
    "form_complete",
    "surgical_menopause_confirmed"
  )

  unknown_keys <- setdiff(names(columns), expected_column_keys)

  if (length(unknown_keys) > 0L) {
    stop(
      "Unrecognized names in `columns`: ",
      paste(unknown_keys, collapse = ", "),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Internal helpers
  # ---------------------------------------------------------------------------

  mapped_name <- function(key) {

    if (!key %in% names(columns)) {
      return(NA_character_)
    }

    value <- unname(columns[[key]])

    if (
      length(value) == 0L ||
      is.na(value) ||
      value == ""
    ) {
      return(NA_character_)
    }

    value
  }

  read_numeric <- function(key, required = FALSE) {

    column_name <- mapped_name(key)

    if (is.na(column_name)) {

      if (required) {
        stop(
          "No column was mapped for required field `",
          key,
          "`.",
          call. = FALSE
        )
      }

      return(rep(NA_real_, nrow(data)))
    }

    if (!column_name %in% names(data)) {

      if (required) {
        stop(
          "Required column `",
          column_name,
          "` was not found in `data`.",
          call. = FALSE
        )
      }

      return(rep(NA_real_, nrow(data)))
    }

    x <- data[[column_name]]

    if (is.factor(x)) {
      x <- as.character(x)
    }

    x_character <- trimws(as.character(x))
    x_character[x_character == ""] <- NA_character_

    x_numeric <- suppressWarnings(as.numeric(x_character))

    failed_conversion <-
      !is.na(x_character) &
      is.na(x_numeric)

    if (any(failed_conversion)) {

      bad_values <- unique(x_character[failed_conversion])

      stop(
        "Column `",
        column_name,
        "` contains nonnumeric response values. ",
        "Use raw response codes or recode the column before scoring. ",
        "Examples: ",
        paste(utils::head(bad_values, 5L), collapse = ", "),
        call. = FALSE
      )
    }

    x_numeric
  }

  read_character <- function(key) {

    column_name <- mapped_name(key)

    if (
      is.na(column_name) ||
      !column_name %in% names(data)
    ) {
      return(rep(NA_character_, nrow(data)))
    }

    x <- trimws(as.character(data[[column_name]]))
    x[x == ""] <- NA_character_

    x
  }

  combine_reasons <- function(reason_data) {

    apply(
      reason_data,
      MARGIN = 1L,
      FUN = function(x) {

        x <- unique(x[!is.na(x) & x != ""])

        if (length(x) == 0L) {
          return(NA_character_)
        }

        paste(x, collapse = "; ")
      }
    )
  }

  # ---------------------------------------------------------------------------
  # Read questionnaire variables
  # ---------------------------------------------------------------------------

  period_past_year <-
    read_numeric("period_past_year")

  last_period <-
    read_numeric("last_period", required = TRUE)

  stopped_reason <-
    read_numeric("stopped_reason")

  stopped_reason_other <-
    read_character("stopped_reason_other")

  skipped_60_days <-
    read_numeric("skipped_60_days", required = TRUE)

  skipped_due_to_age <-
    read_numeric("skipped_due_to_age")

  skipped_reason <-
    read_numeric("skipped_reason")

  skipped_reason_other <-
    read_character("skipped_reason_other")

  repeated_cycle_change <-
    read_numeric(
      "repeated_cycle_change",
      required = TRUE
    )

  bleeding_duration_change <-
    read_numeric(
      "bleeding_duration_change",
      required = TRUE
    )

  bleeding_amount_change <-
    read_numeric(
      "bleeding_amount_change",
      required = TRUE
    )

  hormonal_medication <-
    read_numeric("hormonal_medication")

  form_complete <-
    read_numeric("form_complete")

  surgical_confirmation_raw <-
    read_numeric("surgical_menopause_confirmed")

  # ---------------------------------------------------------------------------
  # Data availability
  # ---------------------------------------------------------------------------

  response_matrix <- cbind(
    period_past_year,
    last_period,
    stopped_reason,
    skipped_60_days,
    skipped_due_to_age,
    skipped_reason,
    repeated_cycle_change,
    bleeding_duration_change,
    bleeding_amount_change,
    hormonal_medication
  )

  data_available_yn <- as.integer(
    rowSums(!is.na(response_matrix)) > 0L
  )

  # ---------------------------------------------------------------------------
  # Candidate reproductive stages
  # ---------------------------------------------------------------------------

  postmenopause_candidate <-
    last_period %in% 1:4

  late_transition_candidate <-
    skipped_60_days == 1

  early_transition_candidate <-
    repeated_cycle_change == 1

  late_reproductive_candidate <-
    bleeding_duration_change == 1 |
    bleeding_amount_change == 1

  reproductive_candidate <-
    bleeding_duration_change == 2 &
    bleeding_amount_change == 2

  candidate_matrix <- cbind(
    postmenopause_candidate,
    late_transition_candidate,
    early_transition_candidate,
    late_reproductive_candidate,
    reproductive_candidate
  )

  candidate_stage_n <- rowSums(
    candidate_matrix,
    na.rm = TRUE
  )

  questionnaire_conflict_yn <- as.integer(
    candidate_stage_n > 1L
  )

  # ---------------------------------------------------------------------------
  # Detailed reproductive stage
  # ---------------------------------------------------------------------------

  stage_detailed <- dplyr::case_when(
    last_period == 1 ~ "Postmenopause +1a",
    last_period == 2 ~ "Postmenopause +1b",
    last_period == 3 ~ "Postmenopause +1c",
    last_period == 4 ~ "Postmenopause +2",

    late_transition_candidate ~
      "Late menopausal transition",

    early_transition_candidate ~
      "Early menopausal transition",

    late_reproductive_candidate ~
      "Late reproductive",

    reproductive_candidate ~
      "Reproductive",

    TRUE ~ NA_character_
  )

  stage_6level <- dplyr::case_when(
    stage_detailed == "Reproductive" ~
      "Early reproductive",

    stage_detailed == "Late reproductive" ~
      "Late reproductive",

    stage_detailed == "Early menopausal transition" ~
      "Early menopausal transition",

    stage_detailed == "Late menopausal transition" ~
      "Late menopausal transition",

    stage_detailed %in% c(
      "Postmenopause +1a",
      "Postmenopause +1b",
      "Postmenopause +1c"
    ) ~ "Early postmenopause",

    stage_detailed == "Postmenopause +2" ~
      "Late postmenopause",

    TRUE ~ NA_character_
  )

  stage_3level <- dplyr::case_when(
    stage_detailed %in% c(
      "Reproductive",
      "Late reproductive"
    ) ~ "Premenopausal",

    stage_detailed %in% c(
      "Early menopausal transition",
      "Late menopausal transition"
    ) ~ "Perimenopausal",

    stage_detailed %in% c(
      "Postmenopause +1a",
      "Postmenopause +1b",
      "Postmenopause +1c",
      "Postmenopause +2"
    ) ~ "Postmenopausal",

    TRUE ~ NA_character_
  )

  stage_order <- dplyr::case_when(
    stage_detailed == "Reproductive" ~ 1L,
    stage_detailed == "Late reproductive" ~ 2L,
    stage_detailed == "Early menopausal transition" ~ 3L,
    stage_detailed == "Late menopausal transition" ~ 4L,
    stage_detailed == "Postmenopause +1a" ~ 5L,
    stage_detailed == "Postmenopause +1b" ~ 6L,
    stage_detailed == "Postmenopause +1c" ~ 7L,
    stage_detailed == "Postmenopause +2" ~ 8L,
    TRUE ~ NA_integer_
  )

  # ---------------------------------------------------------------------------
  # Interpret reasons periods stopped or were skipped
  # ---------------------------------------------------------------------------

  period_stop_reason <- dplyr::case_when(
    stopped_reason == 1 ~
      "Natural menopause",

    stopped_reason == 2 ~
      "Hysterectomy or endometrial ablation",

    stopped_reason == 3 ~
      "Pregnancy or breastfeeding",

    stopped_reason == 4 ~
      "Medication, injection, or hormonal IUD",

    stopped_reason == 5 ~
      "Chemotherapy or cancer treatment",

    stopped_reason == 6 ~
      "Major weight loss, illness, or stress",

    stopped_reason == 7 ~
      "Participant does not know",

    stopped_reason == 8 ~
      "Other reason",

    postmenopause_candidate &
      is.na(stopped_reason) ~
      "Missing reason",

    TRUE ~ NA_character_
  )

  skipped_period_reason <- dplyr::case_when(
    skipped_reason == 1 ~
      "Pregnancy or breastfeeding",

    skipped_reason == 2 ~
      "Hysterectomy or endometrial ablation",

    skipped_reason == 3 ~
      "Medication or birth-control injection",

    skipped_reason == 4 ~
      "Chemotherapy",

    skipped_reason == 5 ~
      "Participant does not know",

    skipped_reason == 6 ~
      "Other reason",

    TRUE ~ NA_character_
  )

  # ---------------------------------------------------------------------------
  # Surgical menopause variables
  # ---------------------------------------------------------------------------

  surgical_response_yn <- as.integer(
    stopped_reason == 2 |
    skipped_reason == 2
  )

  surgical_menopause_reported_yn <- dplyr::case_when(
    surgical_response_yn == 1 &
      surgical_response_is_category ~ 1L,

    surgical_response_yn == 0 ~ 0L,

    TRUE ~ NA_integer_
  )

  surgical_menopause_confirmed_yn <- dplyr::case_when(
    surgical_confirmation_raw == 1 ~ 1L,
    surgical_confirmation_raw == 0 ~ 0L,
    surgical_confirmation_raw == 2 ~ 0L,
    surgical_menopause_reported_yn == 1 ~ NA_integer_,
    TRUE ~ NA_integer_
  )

  # ---------------------------------------------------------------------------
  # Menopause type
  # ---------------------------------------------------------------------------

  menopause_type <- dplyr::case_when(
    surgical_menopause_reported_yn == 1 ~
      "Surgical menopause reported",

    stopped_reason == 1 ~
      "Natural menopause",

    stopped_reason == 3 ~
      "Pregnancy or breastfeeding related",

    stopped_reason == 4 ~
      "Medication-induced amenorrhea",

    stopped_reason == 5 ~
      "Cancer-treatment-induced amenorrhea",

    stopped_reason == 6 ~
      "Illness, weight-loss, or stress-related amenorrhea",

    stopped_reason == 8 ~
      "Other non-natural amenorrhea",

    stopped_reason == 7 ~
      "Unknown menopause type",

    postmenopause_candidate &
      is.na(stopped_reason) ~
      "Missing menopause type",

    stage_3level == "Perimenopausal" ~
      "Not yet postmenopausal",

    stage_3level == "Premenopausal" ~
      "Not yet postmenopausal",

    TRUE ~ NA_character_
  )

  menopause_type <- factor(
    menopause_type,
    levels = c(
      "Not yet postmenopausal",
      "Natural menopause",
      "Surgical menopause reported",
      "Medication-induced amenorrhea",
      "Cancer-treatment-induced amenorrhea",
      "Pregnancy or breastfeeding related",
      "Illness, weight-loss, or stress-related amenorrhea",
      "Other non-natural amenorrhea",
      "Unknown menopause type",
      "Missing menopause type"
    )
  )

  natural_menopause_yn <- dplyr::case_when(
    stopped_reason == 1 ~ 1L,

    stopped_reason %in% c(
      2, 3, 4, 5, 6, 8
    ) ~ 0L,

    TRUE ~ NA_integer_
  )

  nonnatural_menopause_yn <- dplyr::case_when(
    stopped_reason %in% c(
      2, 3, 4, 5, 6, 8
    ) ~ 1L,

    stopped_reason == 1 ~ 0L,

    TRUE ~ NA_integer_
  )

  # ---------------------------------------------------------------------------
  # Hormonal medication
  #
  # BBB coding:
  # 1 = No
  # 2 = Yes, currently using birth control
  # ---------------------------------------------------------------------------

  hormonal_medication_yn <- dplyr::case_when(
    hormonal_medication == 1 ~ 0L,
    hormonal_medication == 2 ~ 1L,
    TRUE ~ NA_integer_
  )

  # ---------------------------------------------------------------------------
  # HCP-style legacy code
  # ---------------------------------------------------------------------------

  unknown_period_stop <-
    stopped_reason == 7 |
    (
      legacy_missing_as_unknown &
      is.na(stopped_reason)
    )

  legacy_code <- dplyr::case_when(
    last_period == 1 & stopped_reason == 1 ~ 1.11,
    last_period == 2 & stopped_reason == 1 ~ 1.12,
    last_period == 3 & stopped_reason == 1 ~ 1.13,
    last_period == 4 & stopped_reason == 1 ~ 1.14,

    last_period == 1 &
      stopped_reason %in% c(2, 3, 5, 6, 8) ~
      1.11002,

    last_period == 2 &
      stopped_reason %in% c(2, 3, 5, 6, 8) ~
      1.12002,

    last_period == 3 &
      stopped_reason %in% c(2, 3, 5, 6, 8) ~
      1.13002,

    last_period == 4 &
      stopped_reason %in% c(2, 3, 5, 6, 8) ~
      1.14002,

    last_period == 1 & stopped_reason == 4 ~
      1.11004,

    last_period == 2 & stopped_reason == 4 ~
      1.12004,

    last_period == 3 & stopped_reason == 4 ~
      1.13004,

    last_period == 4 & stopped_reason == 4 ~
      1.14004,

    last_period == 1 & unknown_period_stop ~
      1.11007,

    last_period == 2 & unknown_period_stop ~
      1.12007,

    last_period == 3 & unknown_period_stop ~
      1.13007,

    last_period == 4 & unknown_period_stop ~
      1.14007,

    skipped_60_days == 1 &
      skipped_due_to_age == 1 ~
      2,

    skipped_60_days == 1 &
      skipped_reason == 3 ~
      2.2003,

    skipped_60_days == 1 &
      skipped_reason %in% c(1, 2, 4, 5, 6) ~
      2.2001,

    skipped_60_days == 1 ~
      2,

    repeated_cycle_change == 1 ~
      3,

    bleeding_duration_change == 1 |
      bleeding_amount_change == 1 ~
      4.1,

    bleeding_duration_change == 2 &
      bleeding_amount_change == 2 ~
      4.2,

    TRUE ~ NA_real_
  )

  # ---------------------------------------------------------------------------
  # Form completion
  # ---------------------------------------------------------------------------

  form_complete_yn <- dplyr::case_when(
    form_complete == 2 ~ 1L,
    form_complete %in% c(0, 1) ~ 0L,
    TRUE ~ NA_integer_
  )

  # ---------------------------------------------------------------------------
  # Manual-review reasons
  # ---------------------------------------------------------------------------

  review_reason_data <- data.frame(
    conflict = dplyr::if_else(
      questionnaire_conflict_yn == 1,
      "Responses support more than one reproductive stage",
      NA_character_
    ),

    unable_to_score = dplyr::if_else(
      data_available_yn == 1 &
        is.na(stage_detailed),
      "Available responses are insufficient to assign a stage",
      NA_character_
    ),

    surgical_unconfirmed = dplyr::if_else(
      require_surgical_confirmation &
        surgical_menopause_reported_yn == 1 &
        is.na(surgical_menopause_confirmed_yn),
      paste0(
        "Surgical menopause category is based on a combined ",
        "hysterectomy/endometrial-ablation response; ",
        "bilateral oophorectomy is not confirmed"
      ),
      NA_character_
    ),

    surgical_not_confirmed = dplyr::if_else(
      require_surgical_confirmation &
        surgical_menopause_reported_yn == 1 &
        surgical_menopause_confirmed_yn == 0,
      paste0(
        "Hysterectomy or endometrial ablation was reported, ",
        "but surgical menopause was not confirmed"
      ),
      NA_character_
    ),

    missing_stop_reason = dplyr::if_else(
      postmenopause_candidate &
        is.na(stopped_reason),
      paste0(
        "Time since last period was reported, but the reason ",
        "periods stopped is missing"
      ),
      NA_character_
    ),

    unknown_stop_reason = dplyr::if_else(
      postmenopause_candidate &
        stopped_reason == 7,
      "Participant does not know why periods stopped",
      NA_character_
    ),

    pregnancy_related = dplyr::if_else(
      stopped_reason == 3,
      "Periods stopped because of pregnancy or breastfeeding",
      NA_character_
    ),

    medication_related = dplyr::if_else(
      stopped_reason == 4,
      "Periods stopped because of medication, injection, or hormonal IUD",
      NA_character_
    ),

    cancer_treatment_related = dplyr::if_else(
      stopped_reason == 5,
      "Periods stopped following chemotherapy or cancer treatment",
      NA_character_
    ),

    illness_related = dplyr::if_else(
      stopped_reason == 6,
      "Periods stopped following major weight loss, illness, or stress",
      NA_character_
    ),

    other_reason = dplyr::if_else(
      stopped_reason == 8,
      "Periods stopped for another specified reason",
      NA_character_
    ),

    other_reason_text_missing = dplyr::if_else(
      stopped_reason == 8 &
        is.na(stopped_reason_other),
      paste0(
        "Other reason was selected, but no explanatory text ",
        "was provided"
      ),
      NA_character_
    ),

    skipped_surgical = dplyr::if_else(
      skipped_reason == 2,
      paste0(
        "Skipped periods were attributed to hysterectomy or ",
        "endometrial ablation"
      ),
      NA_character_
    ),

    skipped_alternative_reason = dplyr::if_else(
      skipped_60_days == 1 &
        skipped_reason %in% c(1, 3, 4, 5, 6),
      paste0(
        "Skipped periods had an alternative explanation: ",
        skipped_period_reason
      ),
      NA_character_
    ),

    skipped_not_due_to_age = dplyr::if_else(
      skipped_60_days == 1 &
        skipped_due_to_age %in% c(2, 3),
      paste0(
        "The 60-day interval without a period was not clearly ",
        "attributed to reproductive aging"
      ),
      NA_character_
    ),

    hormonal_medication = dplyr::if_else(
      hormonal_medication_invalid &
        hormonal_medication_yn == 1,
      "Current hormonal medication may obscure menstrual staging",
      NA_character_
    ),

    incomplete_form = dplyr::if_else(
      require_complete_form &
        form_complete_yn == 0,
      "The STRAW form is not marked complete",
      NA_character_
    ),

    stringsAsFactors = FALSE
  )

  review_reason <- combine_reasons(
    review_reason_data
  )

  manual_review_yn <- as.integer(
    !is.na(review_reason)
  )

  # ---------------------------------------------------------------------------
  # Stage validity
  #
  # Confirmed surgical menopause remains a valid menopause-type category, but
  # the natural STRAW timing stage is not treated as a valid natural-menopause
  # stage.
  # ---------------------------------------------------------------------------

  natural_stage_valid_yn <- dplyr::case_when(
    data_available_yn == 0 ~ NA_integer_,

    is.na(stage_detailed) ~ 0L,

    menopause_type == "Natural menopause" &
      manual_review_yn == 0 ~ 1L,

    stage_3level %in% c(
      "Premenopausal",
      "Perimenopausal"
    ) &
      manual_review_yn == 0 ~ 1L,

    TRUE ~ 0L
  )

  menopause_type_valid_yn <- dplyr::case_when(
    data_available_yn == 0 ~ NA_integer_,

    menopause_type == "Surgical menopause reported" &
      surgical_menopause_confirmed_yn == 1 ~ 1L,

    menopause_type == "Surgical menopause reported" &
      require_surgical_confirmation ~ 0L,

    !is.na(menopause_type) &
      manual_review_yn == 0 ~ 1L,

    !is.na(menopause_type) ~ 0L,

    TRUE ~ 0L
  )

  stage_status <- dplyr::case_when(
    data_available_yn == 0 ~
      "No STRAW data",

    is.na(stage_detailed) ~
      "Unable to score",

    menopause_type ==
      "Surgical menopause reported" &
      surgical_menopause_confirmed_yn == 1 ~
      "Surgical menopause confirmed",

    menopause_type ==
      "Surgical menopause reported" ~
      "Surgical menopause reported, confirmation needed",

    manual_review_yn == 1 ~
      "Scored, manual review required",

    natural_stage_valid_yn == 1 ~
      "Scored and valid",

    TRUE ~
      "Scored, validity uncertain"
  )

  # ---------------------------------------------------------------------------
  # Append outputs
  # ---------------------------------------------------------------------------

  output <- dplyr::mutate(
    data,

    "{paste0(prefix, 'stage_detailed')}" := factor(
      stage_detailed,
      levels = c(
        "Reproductive",
        "Late reproductive",
        "Early menopausal transition",
        "Late menopausal transition",
        "Postmenopause +1a",
        "Postmenopause +1b",
        "Postmenopause +1c",
        "Postmenopause +2"
      ),
      ordered = TRUE
    ),

    "{paste0(prefix, 'stage_6level')}" := factor(
      stage_6level,
      levels = c(
        "Early reproductive",
        "Late reproductive",
        "Early menopausal transition",
        "Late menopausal transition",
        "Early postmenopause",
        "Late postmenopause"
      ),
      ordered = TRUE
    ),

    "{paste0(prefix, 'stage_3level')}" := factor(
      stage_3level,
      levels = c(
        "Premenopausal",
        "Perimenopausal",
        "Postmenopausal"
      ),
      ordered = TRUE
    ),

    "{paste0(prefix, 'stage_order')}" :=
      stage_order,

    "{paste0(prefix, 'menopause_type')}" :=
      menopause_type,

    "{paste0(prefix, 'period_stop_reason')}" :=
      period_stop_reason,

    "{paste0(prefix, 'period_stop_reason_other')}" :=
      stopped_reason_other,

    "{paste0(prefix, 'skipped_period_reason')}" :=
      skipped_period_reason,

    "{paste0(prefix, 'skipped_period_reason_other')}" :=
      skipped_reason_other,

    "{paste0(prefix, 'natural_menopause_yn')}" :=
      natural_menopause_yn,

    "{paste0(prefix, 'nonnatural_menopause_yn')}" :=
      nonnatural_menopause_yn,

    "{paste0(prefix, 'surgical_menopause_reported_yn')}" :=
      surgical_menopause_reported_yn,

    "{paste0(prefix, 'surgical_menopause_confirmed_yn')}" :=
      surgical_menopause_confirmed_yn,

    "{paste0(prefix, 'hormonal_medication_yn')}" :=
      hormonal_medication_yn,

    "{paste0(prefix, 'legacy_code')}" :=
      legacy_code,

    "{paste0(prefix, 'candidate_stage_n')}" :=
      candidate_stage_n,

    "{paste0(prefix, 'questionnaire_conflict_yn')}" :=
      questionnaire_conflict_yn,

    "{paste0(prefix, 'data_available_yn')}" :=
      data_available_yn,

    "{paste0(prefix, 'form_complete_yn')}" :=
      form_complete_yn,

    "{paste0(prefix, 'natural_stage_valid_yn')}" :=
      natural_stage_valid_yn,

    "{paste0(prefix, 'menopause_type_valid_yn')}" :=
      menopause_type_valid_yn,

    "{paste0(prefix, 'manual_review_yn')}" :=
      manual_review_yn,

    "{paste0(prefix, 'review_reason')}" :=
      review_reason,

    "{paste0(prefix, 'stage_status')}" :=
      stage_status
  )

  # ---------------------------------------------------------------------------
  # Optional variable labels
  # ---------------------------------------------------------------------------

  if (
    add_variable_labels &&
    requireNamespace("sjlabelled", quietly = TRUE)
  ) {

    labels <- c(
      stage_detailed =
        "Questionnaire-derived detailed STRAW reproductive aging stage",

      stage_6level =
        "Questionnaire-derived six-level reproductive aging stage",

      stage_3level =
        "Questionnaire-derived broad reproductive aging stage",

      stage_order =
        "Ordered numeric reproductive aging stage",

      menopause_type =
        "Questionnaire-derived menopause or amenorrhea type",

      period_stop_reason =
        "Reported reason periods stopped",

      period_stop_reason_other =
        "Other specified reason periods stopped",

      skipped_period_reason =
        "Reported reason periods were skipped",

      skipped_period_reason_other =
        "Other specified reason periods were skipped",

      natural_menopause_yn =
        "Natural menopause explicitly reported",

      nonnatural_menopause_yn =
        "Non-natural cause of amenorrhea reported",

      surgical_menopause_reported_yn =
        "Surgical menopause category reported from hysterectomy or ablation response",

      surgical_menopause_confirmed_yn =
        "Surgical menopause independently confirmed",

      hormonal_medication_yn =
        "Current hormonal medication reported",

      legacy_code =
        "Legacy HCP-style STRAW questionnaire code",

      candidate_stage_n =
        "Number of reproductive stages supported by responses",

      questionnaire_conflict_yn =
        "Responses support more than one reproductive stage",

      data_available_yn =
        "At least one STRAW questionnaire response is available",

      form_complete_yn =
        "STRAW REDCap form is marked complete",

      natural_stage_valid_yn =
        "Natural STRAW stage valid without adjudication",

      menopause_type_valid_yn =
        "Menopause type valid without adjudication",

      manual_review_yn =
        "STRAW assessment requires manual review",

      review_reason =
        "Reason STRAW assessment requires manual review",

      stage_status =
        "Overall STRAW scoring status"
    )

    for (suffix in names(labels)) {

      variable_name <- paste0(prefix, suffix)

      output[[variable_name]] <-
        sjlabelled::set_label(
          output[[variable_name]],
          label = unname(labels[[suffix]])
        )
    }
  }

  output
}