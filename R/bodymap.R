#' Plot a 5-D Itch Scale anatomical distribution map
#'
#' Uses a built-in schematic front/back body-region template and hand/foot
#' insets aligned to the 16 distribution choices of the 5-D Itch Scale. This
#' is a region-level visualization, not a lesion locator or questionnaire copy.
#'
#' @param data Participant-level data with binary 0/1 or logical region columns.
#' @param region_cols Named vector mapping the 16 canonical region IDs to the
#'   actual data-frame column names. Names must be canonical IDs returned by
#'   [itch_anatomy_regions()]. Unnamed input is allowed only when names already
#'   match the canonical IDs.
#' @param denominator `available` excludes missing responses per region;
#'   `all` uses all rows as denominator.
#' @param show_labels Show region labels on the map.
#' @param title Plot title.
#' @return A ggplot object with front and back panels plus hand/foot details.
#' @export
plot_itch_bodymap <- function(data, region_cols = itch_anatomy_regions(),
                              denominator = c("available", "all"),
                              show_labels = TRUE,
                              title = "5-D Itch Scale: itch distribution by body region") {
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

  # Each polygon is an intentionally simplified anatomical region. Coordinates
  # are schematic and do not represent precise clinical borders.
  poly <- function(id, view, x, y, label_x = mean(range(x)), label_y = mean(range(y))) {
    data.frame(region_id = id, view = view, polygon_id = paste(id, view, sep = "_"),
               x = x, y = y, label_x = label_x, label_y = label_y)
  }
  P <- list(
    # Front silhouette and region overlays
    poly("head_scalp","Front",c(-.24,-.24,-.18,-.12,-.06,.00,.06,.12,.18,.24,.24,.18,.12,.06,0,-.06,-.12,-.18),c(9.55,9.83,10.02,10.13,10.18,10.20,10.18,10.13,10.02,9.83,9.55,9.39,9.30,9.26,9.25,9.26,9.30,9.39),0,9.78),
    poly("face","Front",c(-.16,-.16,-.11,-.06,0,.06,.11,.16,.16,.11,.06,0,-.06,-.11),c(9.68,9.48,9.34,9.29,9.28,9.29,9.34,9.48,9.61,9.67,9.69,9.70,9.69,9.67),0,9.49),
    poly("upper_arms","Front",c(-.40,-.56,-.64,-.60,-.50,-.43,-.34,-.30,-.30,.30,.30,.34,.43,.50,.60,.64,.56,.40),c(8.92,8.83,8.30,7.94,7.96,8.38,8.68,8.80,8.92,8.92,8.80,8.68,8.38,7.96,7.94,8.30,8.83,8.92),-.49,8.42),
    poly("forearms","Front",c(-.60,-.70,-.76,-.72,-.62,-.55,-.49,-.43,-.39,-.35,.35,.39,.43,.49,.55,.62,.72,.76,.70,.60),c(7.94,7.72,7.14,6.86,6.90,7.10,7.58,7.92,8.18,8.18,8.18,8.18,7.92,7.58,7.10,6.90,6.86,7.14,7.72,7.94),-.59,7.43),
    poly("chest","Front",c(-.31,-.30,-.24,-.15,0,.15,.24,.30,.31,.25,.18,0,-.18,-.25),c(8.86,8.44,8.23,8.16,8.13,8.16,8.23,8.44,8.86,8.93,8.97,8.98,8.97,8.93),0,8.65),
    poly("abdomen","Front",c(-.25,-.24,-.22,-.20,-.19,0,.19,.20,.22,.24,.25,.22,.16,0,-.16,-.22),c(8.42,8.18,7.90,7.60,7.38,7.32,7.38,7.60,7.90,8.18,8.42,8.46,8.40,8.38,8.40,8.46),0,7.88),
    poly("groin","Front",c(-.20,-.16,-.09,0,.09,.16,.20,.13,.07,0,-.07,-.13),c(7.40,7.18,6.95,6.88,6.95,7.18,7.40,7.45,7.42,7.40,7.42,7.45),0,7.15),
    poly("thighs","Front",c(-.20,-.10,-.02,.02,.10,.20,.25,.25,.22,.16,.09,.02,-.02,-.09,-.16,-.22,-.25,-.25),c(7.30,7.28,7.28,7.28,7.28,7.30,6.98,6.45,5.92,5.65,5.58,5.58,5.58,5.58,5.65,5.92,6.45,6.98),-.13,6.48),
    poly("lower_legs","Front",c(-.22,-.17,-.12,-.07,-.02,.02,.07,.12,.17,.22,.19,.15,.10,.06,.02,-.02,-.06,-.10,-.15,-.19),c(5.92,5.60,5.20,4.65,4.20,4.20,4.65,5.20,5.60,5.92,4.00,3.55,3.30,3.22,3.20,3.20,3.22,3.30,3.55,4.00),0,4.75),
    poly("feet_top","Front",c(-.15,-.10,-.04,0,.04,.10,.15,.19,.17,.12,.05,0,-.05,-.12,-.17,-.19),c(3.55,3.48,3.40,3.39,3.40,3.48,3.55,3.36,3.14,3.03,2.98,2.98,2.98,3.03,3.14,3.36),0,3.22),
    poly("back","Back",c(-.30,-.30,-.24,-.18,0,.18,.24,.30,.30,.25,.20,.16,0,-.16,-.20,-.25),c(8.90,8.43,8.14,7.96,7.90,7.96,8.14,8.43,8.90,8.92,8.65,8.45,8.40,8.45,8.65,8.92),0,8.47),
    poly("buttocks","Back",c(-.24,-.22,-.12,0,.12,.22,.24,.18,.09,0,-.09,-.18),c(7.95,7.62,7.43,7.42,7.43,7.62,7.95,8.03,8.04,8.02,8.04,8.03),0,7.73),
    poly("upper_arms","Back",c(-.40,-.56,-.64,-.60,-.50,-.43,-.34,-.30,-.30,.30,.30,.34,.43,.50,.60,.64,.56,.40),c(8.92,8.83,8.30,7.94,7.96,8.38,8.68,8.80,8.92,8.92,8.80,8.68,8.38,7.96,7.94,8.30,8.83,8.92),-.49,8.42),
    poly("forearms","Back",c(-.60,-.70,-.76,-.72,-.62,-.55,-.49,-.43,-.39,-.35,.35,.39,.43,.49,.55,.62,.72,.76,.70,.60),c(7.94,7.72,7.14,6.86,6.90,7.10,7.58,7.92,8.18,8.18,8.18,8.18,7.92,7.58,7.10,6.90,6.86,7.14,7.72,7.94),-.59,7.43),
    poly("thighs","Back",c(-.20,-.10,-.02,.02,.10,.20,.25,.25,.22,.16,.09,.02,-.02,-.09,-.16,-.22,-.25,-.25),c(7.30,7.28,7.28,7.28,7.28,7.30,6.98,6.45,5.92,5.65,5.58,5.58,5.58,5.58,5.65,5.92,6.45,6.98),-.13,6.48),
    poly("lower_legs","Back",c(-.22,-.17,-.12,-.07,-.02,.02,.07,.12,.17,.22,.19,.15,.10,.06,.02,-.02,-.06,-.10,-.15,-.19),c(5.92,5.60,5.20,4.65,4.20,4.20,4.65,5.20,5.60,5.92,4.00,3.55,3.30,3.22,3.20,3.20,3.22,3.30,3.55,4.00),0,4.75),
    # Contact clothing/bandage is shown as an overlay band, not a separate body part
    poly("clothing_contact","Front",c(-.25,-.24,.24,.25,.22,-.22),c(7.92,7.78,7.78,7.92,8.00,8.00),0,7.86),
    poly("clothing_contact","Back",c(-.25,-.24,.24,.25,.22,-.22),c(7.92,7.78,7.78,7.92,8.00,8.00),0,7.86),
    # Insets for regions not reliably visible on a whole-body silhouette
    poly("palms","Hands / feet",c(-.82,-.62,-.62,-.82),c(8.8,8.8,8.35,8.35),-.72,8.57),
    poly("hands_back","Hands / feet",c(.62,.82,.82,.62),c(8.8,8.8,8.35,8.35),.72,8.57),
    poly("soles","Hands / feet",c(-.82,-.62,-.62,-.82),c(3.55,3.55,3.05,3.05),-.72,3.30),
    poly("feet_top","Hands / feet",c(.62,.82,.82,.62),c(3.55,3.55,3.05,3.05),.72,3.30)
  )
  polygons <- do.call(rbind, P)
  polygons$percent <- unname(prevalence[polygons$region_id])
  polygons$region_label <- unname(itch_anatomy_regions()[polygons$region_id])
  # Fill region polygons by prevalence; polygons are separated by view and ID.
  p <- ggplot2::ggplot(polygons, ggplot2::aes(
    x = x, y = y, group = interaction(view, polygon_id), fill = percent
  )) +
    ggplot2::geom_polygon(color = "white", linewidth = 0.45, na.rm = FALSE) +
    ggplot2::facet_wrap(~view, nrow = 1) +
    ggplot2::scale_fill_viridis_c(limits = c(0, 100), na.value = "grey82",
                                  name = "Participants\nreporting itch (%)") +
    ggplot2::coord_equal(xlim = c(-1.0, 1.0), ylim = c(2.7, 10.3), expand = FALSE) +
    ggplot2::labs(title = title,
      subtitle = "Schematic region-level visualization; not precise lesion boundaries",
      x = NULL, y = NULL,
      caption = paste0("Denominator: ", denominator,
        ". Clothing/bandage contact is an overlay category, not an anatomical body region.")) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold", size = 12),
      legend.position = "right")
  if (show_labels) {
    labs <- unique(polygons[, c("region_id","region_label","view","percent")])
    # Place labels within selected large regions to avoid obscuring the outline.
    positions <- data.frame(
      region_id = c("head_scalp","face","chest","abdomen","back","buttocks",
                    "thighs","lower_legs","feet_top","upper_arms","forearms",
                    "groin","clothing_contact","palms","hands_back","soles","feet_top"),
      view = c("Front","Front","Front","Front","Back","Back","Front","Front",
               "Front","Front","Front","Front","Front","Hands / feet","Hands / feet",
               "Hands / feet","Hands / feet"),
      x = c(0,0,0,0,0,0,-.13,0,0,-.49,-.59,0,0,-.72,.72,-.72,.72),
      y = c(9.78,9.49,8.65,7.88,8.47,7.73,6.48,4.75,3.22,8.42,7.43,7.15,7.86,8.57,8.57,3.30,3.30)
    )
    labs <- merge(labs, positions, by = c("region_id","view"), all.x = TRUE)
    labs <- labs[!is.na(labs$x), , drop = FALSE]
    labs$label <- paste0(labs$region_label, "\n", ifelse(is.na(labs$percent), "NA", paste0(round(labs$percent), "%")))
    p <- p + ggplot2::geom_text(data = labs, ggplot2::aes(x = x, y = y, label = label),
                                inherit.aes = FALSE, size = 2.7, lineheight = .9)
  }
  p
}

#' Canonical region IDs and labels for the 5-D Itch Scale
#'
#' Clothing/bandage contact is a questionnaire category, not an anatomical
#' body part. Verify wording against the instrument version used in the study.
#' @return Named character vector with 16 canonical IDs and display labels.
#' @export
itch_anatomy_regions <- function() {
  c(head_scalp = "Head / scalp",
    face = "Face",
    chest = "Chest",
    abdomen = "Abdomen",
    back = "Back",
    buttocks = "Buttocks",
    thighs = "Thighs",
    lower_legs = "Lower legs",
    feet_top = "Tops of feet / toes",
    soles = "Soles",
    palms = "Palms",
    hands_back = "Tops of hands / fingers",
    forearms = "Forearms",
    upper_arms = "Upper arms",
    clothing_contact = "Contact with clothing / bandages",
    groin = "Groin")
}
