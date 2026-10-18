#' Plot itch prevalence by region
#'
#' @param data Data frame with one row per participant.
#' @param region_cols Names of region columns.
#' @param denominator See [itch_prevalence()].
#' @param show_counts Add affected/denominator labels.
#' @param title Plot title.
#' @return A ggplot object.
#' @export
plot_itch_prevalence <- function(data, region_cols = itch_regions(), denominator = c("available", "all"), show_counts = TRUE, title = "Itch distribution by body region") {
  denominator <- match.arg(denominator)
  d <- itch_prevalence(data, region_cols, denominator)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = percent, y = region)) +
    ggplot2::geom_col(width = .72, fill = "#287D8E", na.rm = TRUE) +
    ggplot2::scale_x_continuous(breaks = seq(0, 100, 20), labels = function(x) paste0(x, "%"),
                                expand = ggplot2::expansion(mult = c(0, .16))) +
    ggplot2::labs(title = title, x = "Participants reporting itch in region", y = NULL,
                  caption = paste("Denominator:", denominator)) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold"))
  if (show_counts) {
    d$label <- ifelse(is.na(d$percent), "No data", paste0(d$affected, "/", d$denominator))
    p <- p + ggplot2::geom_text(data = d, ggplot2::aes(label = label), hjust = -.15, size = 3.2, na.rm = TRUE)
  }
  p
}

#' Plot a 16-region tile summary
#'
#' Tiles are a schematic summary and are not an anatomical body silhouette.
#' @param data Data frame with one row per participant.
#' @param region_cols Names of region columns.
#' @param denominator See [itch_prevalence()].
#' @return A ggplot object.
#' @export
plot_itch_tiles <- function(data, region_cols = itch_regions(), denominator = c("available", "all")) {
  denominator <- match.arg(denominator)
  d <- itch_prevalence(data, region_cols, denominator)
  d$index <- seq_len(nrow(d))
  d$column <- (d$index - 1) %% 4 + 1
  d$row <- 4 - floor((d$index - 1) / 4)
  d$region <- as.character(d$region)
  ggplot2::ggplot(d, ggplot2::aes(x = column, y = row, fill = percent)) +
    ggplot2::geom_tile(color = "white", linewidth = 1.2, width = .96, height = .96) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(region, "\n", ifelse(is.na(percent), "NA", paste0(round(percent), "%")))), size = 3.1) +
    ggplot2::scale_fill_viridis_c(limits = c(0, 100), na.value = "grey85", name = "Affected (%)") +
    ggplot2::scale_x_continuous(breaks = NULL) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "Itch distribution across 16 regions",
                  subtitle = "Schematic tiles; not anatomical body areas",
                  x = NULL, y = NULL, caption = paste("Denominator:", denominator)) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), legend.position = "right")
}
