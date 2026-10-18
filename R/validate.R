#' Validate itch-region data
#'
#' @param data Data frame with one row per participant.
#' @param region_cols Names of the region columns.
#' @return Invisible list with `valid`, `errors`, and `warnings`.
#' @export
validate_itch_data <- function(data, region_cols = itch_regions()) {
  errors <- character()
  warnings <- character()
  if (!is.data.frame(data)) {
    return(invisible(list(valid = FALSE, errors = "data must be a data.frame", warnings = warnings)))
  }
  missing_cols <- setdiff(region_cols, names(data))
  if (length(missing_cols)) {
    errors <- c(errors, paste0("Missing columns: ", paste(missing_cols, collapse = ", ")))
  }
  for (nm in intersect(region_cols, names(data))) {
    x <- data[[nm]]
    if (!(is.logical(x) || is.numeric(x) || is.integer(x))) {
      errors <- c(errors, paste0("Column '", nm, "' must be logical or numeric 0/1."))
    } else if (length(setdiff(unique(x[!is.na(x)]), c(0, 1, FALSE, TRUE)))) {
      errors <- c(errors, paste0("Column '", nm, "' contains values other than 0/1 or NA."))
    }
  }
  if (!nrow(data)) warnings <- c(warnings, "data has zero rows")
  result <- list(valid = !length(errors), errors = errors, warnings = warnings)
  if (length(errors)) warning(paste(errors, collapse = "\n"), call. = FALSE)
  invisible(result)
}
