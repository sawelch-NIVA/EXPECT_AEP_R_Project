# Imputation functions ----

#' Impute values below detection/quantification limits
#'
#' @description
#' Imputes missing values using a specified imputation function, with priority
#' given to LOD values over LOQ values when both are present.
#'
#' @param data A data frame containing the columns to impute
#' @param measured_col Character. Name of column containing measured values
#' @param lod_col Character. Name of column containing limit of detection values
#' @param loq_col Character. Name of column containing limit of quantification values
#' @param impute_fn Function. Function to apply to LOD/LOQ values for imputation.
#'   Default is \code{function(x) x / sqrt(2)}
#' @param output_col Character. Name for the output column containing
#'   measured or imputed values. Default is "MEASURED_OR_IMPUTED_VALUE"
#'
#' @return Data frame with added column containing measured or imputed values
#'
#' @details
#' The function follows this priority order:
#' 1. Use measured value if available
#' 2. Use imputed LOD value if available
#' 3. Use imputed LOQ value if available
#' 4. Return NA if none available
#'
#' @examples
#' df <- data.frame(
#'   measured = c(5, NA, NA),
#'   lod = c(NA, 2, NA),
#'   loq = c(NA, NA, 3)
#' )
#' impute_below_limits(df, "measured", "lod", "loq")
#'
#' @export
impute_below_limits <- function(
  data,
  measured_col,
  lod_col,
  loq_col,
  impute_fn = function(x) x / sqrt(2), # obviously this is Bad Practice but what can you do
  output_col = "MEASURED_OR_IMPUTED_VALUE"
) {
  # Apply imputation function
  data[[paste0(lod_col, "_IMPUTED")]] <- impute_fn(data[[lod_col]])
  data[[paste0(loq_col, "_IMPUTED")]] <- impute_fn(data[[loq_col]])

  # Prioritize: measured > LOD imputed > LOQ imputed > NA
  data[[output_col]] <- dplyr::case_when(
    !is.na(data[[measured_col]]) ~ data[[measured_col]],
    !is.na(data[[paste0(lod_col, "_IMPUTED")]]) ~ data[[paste0(
      lod_col,
      "_IMPUTED"
    )]],
    !is.na(data[[paste0(loq_col, "_IMPUTED")]]) ~ data[[paste0(
      loq_col,
      "_IMPUTED"
    )]],
    .default = NA_real_
  )

  return(data)
}

# it's irrelevant here because the paper reported below LODs but not the actual values, but still...
# https://bookdown.org/mj_rolland/sepages_pipeline_doc/imputation-of-values-below-lod.html
# https://pmc.ncbi.nlm.nih.gov/articles/PMC4838401/
