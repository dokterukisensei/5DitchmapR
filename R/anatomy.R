#' Plot itch prevalence on a supplied anatomical map
#'
#' Join region-level prevalence to a user-supplied anatomical polygon data frame.
#' The polygon data must be appropriately licensed and validated for the
#' questionnaire regions used in the study.
#'
#' @param data Participant-level data with one binary column per region.
#' @param polygons Data frame with `region`, `x`, and `y` columns.
#'   `region` must exactly match names in `region_cols`. Optional columns:
#'   `polygon_id` identifies separate polygons; `view` labels views such as
#'   `Front` and `Back` and creates separate panels.
#' @param region_cols Region column names.
#' @param denominator See [itch_prevalence()].
#' @param title Plot title.
#' @return A ggplot object.
#' @export
plot_itch_anatomy <- function(data, polygons, region_cols = itch_regions(),
                              denominator = c("available", "all"),
                              title = "Itch prevalence by anatomical region") {
  denominator <- match.arg(denominator)
  required <- c("region", "x", "y")
  missing <- setdiff(required, names(polygons))
  if (length(missing)) {
    stop("polygons must contain columns: ", paste(required, collapse = ", "),
         call. = FALSE)
  }
  if (!is.data.frame(polygons) || !nrow(polygons)) {
    stop("polygons must be a non-empty data.frame.", call. = FALSE)
  }
  if (!is.numeric(polygons$x) || !is.numeric(polygons$y)) {
    stop("polygons$x and polygons$y must be numeric coordinates.", call. = FALSE)
  }
  unknown <- setdiff(unique(as.character(polygons$region)), region_cols)
  if (length(unknown)) {
    stop("Polygon region values not found in region_cols: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  polygons$.vertex_order_internal <- seq_len(nrow(polygons))
  if (!"polygon_id" %in% names(polygons)) {
    polygons$polygon_id <- as.character(polygons$region)
  }
  if (!"view" %in% names(polygons)) polygons$view <- "Body"
  prev <- itch_prevalence(data, region_cols, denominator)
  prev$region <- as.character(prev$region)
  map_data <- merge(polygons, prev[, c("region", "percent", "affected", "denominator")],
                    by = "region", all.x = TRUE, sort = FALSE)
  map_data <- map_data[order(map_data$.vertex_order_internal), , drop = FALSE]
  map_data$.vertex_order_internal <- NULL
  map_data$view <- factor(map_data$view, levels = unique(map_data$view))
  ggplot2::ggplot(map_data, ggplot2::aes(
    x = x, y = y, group = interaction(view, polygon_id), fill = percent
  )) +
    ggplot2::geom_polygon(color = "white", linewidth = 0.25, na.rm = FALSE) +
    ggplot2::facet_wrap(~view) +
    ggplot2::scale_fill_viridis_c(
      limits = c(0, 100), na.value = "grey85", name = "Itch reporting (%)"
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = title,
      subtitle = "Color indicates the percentage reporting itch in each mapped region",
      x = NULL, y = NULL,
      caption = paste("Denominator:", denominator,
                      "| Grey indicates regions with no available prevalence.")
    ) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )
}
