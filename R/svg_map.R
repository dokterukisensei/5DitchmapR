#' Write a data-driven 5-D Itch Scale map using the detailed SVG template
#'
#' Creates a copy of the bundled front/back anatomical illustration and colors
#' the broad body-location overlays by the prevalence of itch in each region.
#' The output is an SVG file that can be opened in a browser or edited in a
#' vector graphics program.
#'
#' @param data Participant-level data with binary 0/1 or logical region columns.
#' @param region_cols Named vector mapping canonical IDs from
#'   [itch_anatomy_regions()] to data-frame column names.
#' @param denominator `available` excludes missing responses per region;
#'   `all` uses all rows as denominator.
#' @param output_file File path for the generated SVG. Defaults to a temporary
#'   SVG file.
#' @return The path to the generated SVG file, invisibly.
#' @export
write_itch_bodymap_svg <- function(data, region_cols = itch_anatomy_regions(),
                                   denominator = c("available", "all"),
                                   output_file = tempfile(fileext = ".svg")) {
  denominator <- match.arg(denominator)
  canonical <- names(itch_anatomy_regions())
  if (is.null(names(region_cols)) || any(names(region_cols) == "")) {
    if (all(canonical %in% as.character(region_cols))) {
      names(region_cols) <- as.character(region_cols)
    } else {
      stop("region_cols must be a named vector mapping canonical region IDs to data columns. See itch_anatomy_regions().", call. = FALSE)
    }
  }
  if (!all(canonical %in% names(region_cols))) {
    stop("region_cols must map all 16 canonical region IDs. See itch_anatomy_regions().", call. = FALSE)
  }
  cols <- unname(region_cols[canonical])
  names(cols) <- canonical
  prev <- itch_prevalence(data, cols, denominator)
  prev$region <- canonical
  prevalence <- stats::setNames(prev$percent, prev$region)

  template <- itch_anatomical_template()
  svg <- paste(readLines(template, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  palette <- scales::col_numeric(
    palette = "viridis", domain = c(0, 100), na.color = "#D9D9D9"
  )
  shape_region <- c(
    "head_scalp" = "head_scalp",
    "face" = "face",
    "upper_arms" = "upper_arms", "upper_arms-back" = "upper_arms",
    "forearms" = "forearms", "forearms-back" = "forearms",
    "chest" = "chest", "abdomen" = "abdomen", "groin" = "groin",
    "thighs-front" = "thighs", "thighs-back" = "thighs",
    "lower_legs-front" = "lower_legs", "lower_legs-back" = "lower_legs",
    "feet_top-front" = "feet_top", "back" = "back",
    "buttocks" = "buttocks", "clothing_contact" = "clothing_contact",
    "palms" = "palms", "hands_back" = "hands_back", "soles" = "soles"
  )
  for (shape in names(shape_region)) {
    region <- unname(shape_region[[shape]])
    value <- unname(prevalence[[region]])
    colour <- if (length(value) == 0L || is.na(value)) "#D9D9D9" else palette(value)
    pattern <- paste0('(<path id="itch-', shape, '"[^>]*fill=")[^"]+(")')
    replacement <- paste0("\\1", colour, "\\2")
    svg <- sub(pattern, replacement, svg, perl = TRUE)
  }
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(svg, output_file, useBytes = TRUE)
  invisible(normalizePath(output_file, mustWork = TRUE))
}
