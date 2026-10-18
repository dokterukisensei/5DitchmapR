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
    palette = grDevices::colorRampPalette(c("#440154", "#21918c", "#fde725"))(101),
    domain = c(0, 100), na.color = "#D9D9D9"
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
  # Add a legend outside the original illustration canvas.
  svg <- sub('viewBox="0 0 1077.2 1275.6"',
             'viewBox="0 0 1450 1275.6"', svg, fixed = TRUE)
  labels <- unname(itch_anatomy_regions()[canonical])
  legend_rows <- vapply(seq_along(canonical), function(i) {
    region <- canonical[[i]]
    value <- unname(prevalence[[region]])
    colour <- if (length(value) == 0L || is.na(value)) "#D9D9D9" else palette(value)
    value_label <- if (length(value) == 0L || is.na(value)) "NA" else paste0(round(value), "%")
    label <- gsub("&", "&amp;", labels[[i]], fixed = TRUE)
    label <- gsub("<", "&lt;", label, fixed = TRUE)
    label <- gsub(">", "&gt;", label, fixed = TRUE)
    y <- 95 + (i - 1) * 66
    paste0(
      '<rect x="1090" y="', y - 19, '" width="22" height="22" rx="3" fill="',
      colour, '" stroke="#344054" stroke-width="1"/>',
      '<text x="1125" y="', y, '" font-family="Arial, sans-serif" font-size="18" fill="#202124">',
      label, ' — ', value_label, '</text>'
    )
  }, character(1))
  legend <- paste0(
    '<g id="ditchmapr-legend">',
    '<text x="1090" y="42" font-family="Arial, sans-serif" font-size="24" font-weight="bold" fill="#202124">Itch distribution</text>',
    '<text x="1090" y="68" font-family="Arial, sans-serif" font-size="15" fill="#555">Participants reporting itch</text>',
    paste(legend_rows, collapse = ""),
    '</g>'
  )
  svg <- sub("</svg>", paste0(legend, "</svg>"), svg, fixed = TRUE)
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(svg, output_file, useBytes = TRUE)
  invisible(normalizePath(output_file, mustWork = TRUE))
}
