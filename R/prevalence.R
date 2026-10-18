#' Calculate region-specific itch prevalence
#'
#' @param data Data frame with one row per participant.
#' @param region_cols Names of the region columns.
#' @param denominator `available` uses non-missing answers per region;
#'   `all` uses all rows and treats missing answers as not affected.
#' @return Data frame containing affected, denominator, prevalence, and percent.
#' @export
itch_prevalence <- function(data, region_cols = itch_regions(), denominator = c("available", "all")) {
  denominator <- match.arg(denominator)
  check <- validate_itch_data(data, region_cols)
  if (!check$valid) stop(paste(check$errors, collapse = "\n"), call. = FALSE)
  rows <- lapply(region_cols, function(nm) {
    x <- as.numeric(data[[nm]])
    n <- if (denominator == "available") sum(!is.na(x)) else length(x)
    k <- sum(x == 1, na.rm = TRUE)
    data.frame(region = nm, affected = k, denominator = n,
               prevalence = if (n) k / n else NA_real_,
               percent = if (n) 100 * k / n else NA_real_)
  })
  out <- do.call(rbind, rows)
  out$region <- factor(out$region, levels = rev(region_cols))
  out
}
