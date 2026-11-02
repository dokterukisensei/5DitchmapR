# Supplementary Figure S2: anatomical distribution of pruritus
# Repository: dokterukisensei/5DitchmapR
#
# Produces two separate, publication-quality files:
#   S2A: anterior/posterior anatomical map
#   S2B: prevalence across the individual 5-D Itch Scale locations
#
# Required objects in the calling R session:
#   data_hd_97     - conventional-HD analysis dataset (one row per patient)
#   output_directory - existing output directory
#
# Required input columns are listed in site_dictionary below.
# Responses are converted using the following convention:
#   1 / TRUE / Yes = itch present; 0 / FALSE / No = absent.
# Missing or unrecognized responses remain missing.
#
# Note: This script uses the bodymap package's broad anatomical polygons.
# The individual questionnaire locations remain fully represented in S2B.
# Groin, buttocks, and clothing-contact locations are not forced onto
# anatomically non-equivalent polygons in S2A.

# ------------------------------------------------------------
# 0. Packages and input checks
# ------------------------------------------------------------

required_packages <- c(
  "dplyr", "tidyr", "purrr", "ggplot2", "sf", "tibble",
  "scales", "stringr", "grid", "viridis"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop(
    "Please install required packages first: ",
    paste(missing_packages, collapse = ", ")
  )
}

if (!requireNamespace("bodymap", quietly = TRUE)) {
  stop("Install the bodymap package before running this script.")
}

if (!exists("data_hd_97", inherits = TRUE)) {
  stop("Object 'data_hd_97' was not found.")
}

if (!exists("output_directory", inherits = TRUE)) {
  stop("Object 'output_directory' was not found.")
}

if (!dir.exists(output_directory)) {
  stop("The output_directory does not exist: ", output_directory)
}

if (!is.data.frame(data_hd_97) || nrow(data_hd_97) == 0) {
  stop("'data_hd_97' must be a non-empty data frame.")
}

# ------------------------------------------------------------
# 1. Response conversion
# ------------------------------------------------------------

to_binary_flag_s2 <- function(x) {
  if (is.logical(x)) {
    return(ifelse(is.na(x), NA_real_, as.numeric(x)))
  }

  x_chr <- tolower(trimws(as.character(x)))
  x_num <- suppressWarnings(as.numeric(x_chr))
  out <- rep(NA_real_, length(x_chr))

  out[!is.na(x_num) & x_num == 1] <- 1
  out[!is.na(x_num) & x_num == 0] <- 0

  out[x_chr %in% c("yes", "y", "true", "present", "used", "use")] <- 1
  out[x_chr %in% c("no", "n", "false", "absent", "not used", "none")] <- 0

  out
}

# ------------------------------------------------------------
# 2. Define individual questionnaire locations
# ------------------------------------------------------------

site_dictionary <- tibble::tribble(
  ~variable,             ~site,                    ~order,
  "itch_head_scalp",     "Head/scalp",                  1,
  "itch_face",           "Face",                        2,
  "itch_chest",          "Chest",                       3,
  "itch_abdomen",        "Abdomen",                     4,
  "itch_back",           "Back",                        5,
  "itch_buttocks",       "Buttocks",                    6,
  "itch_thigh",          "Thighs",                      7,
  "itch_lower_leg",      "Lower legs",                  8,
  "itch_foot_top_toes",  "Feet/toes",                   9,
  "itch_sole",           "Soles",                      10,
  "itch_palm",           "Palms",                      11,
  "itch_hand_fingers",   "Hands/fingers",              12,
  "itch_forearm",        "Forearms",                   13,
  "itch_upper_arm",      "Upper arms",                 14,
  "itch_clothing_area",  "Clothing-contact areas",     15,
  "itch_groin",          "Groin",                      16
)

available_sites <- site_dictionary %>%
  dplyr::filter(.data$variable %in% names(data_hd_97))

missing_sites <- setdiff(
  site_dictionary$variable,
  available_sites$variable
)

if (length(missing_sites)) {
  warning(
    "These questionnaire variables were not found and will be omitted: ",
    paste(missing_sites, collapse = ", ")
  )
}

if (nrow(available_sites) == 0) {
  stop("None of the expected anatomical questionnaire variables were found.")
}

# ------------------------------------------------------------
# 3. Calculate prevalence for each individual location
# ------------------------------------------------------------

site_results <- purrr::map_dfr(seq_len(nrow(available_sites)), function(i) {
  variable_i <- available_sites$variable[i]
  flag <- to_binary_flag_s2(data_hd_97[[variable_i]])

  denominator <- sum(!is.na(flag))
  n_affected <- sum(flag == 1, na.rm = TRUE)

  tibble::tibble(
    variable = variable_i,
    site = available_sites$site[i],
    order = available_sites$order[i],
    n = n_affected,
    denominator = denominator,
    percentage = if (denominator > 0) {
      100 * n_affected / denominator
    } else {
      NA_real_
    }
  )
}) %>%
  dplyr::arrange(.data$order)

utils::write.csv(
  site_results,
  file = file.path(
    output_directory,
    "Supplementary_Table_S2_All_16_Anatomical_Sites.csv"
  ),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------
# 4. Aggregate broad regions for the anatomical map
#    Each patient is counted once per broad region.
# ------------------------------------------------------------

get_any_site_s2 <- function(vars, dat = data_hd_97) {
  vars <- intersect(vars, names(dat))

  if (!length(vars)) {
    return(rep(NA_real_, nrow(dat)))
  }

  mat <- do.call(
    cbind,
    lapply(vars, function(v) to_binary_flag_s2(dat[[v]]))
  )

  if (is.null(dim(mat))) {
    mat <- matrix(mat, ncol = 1)
  }

  has_response <- rowSums(!is.na(mat)) > 0
  result <- rep(NA_real_, nrow(mat))

  result[has_response] <- as.numeric(
    rowSums(mat == 1, na.rm = TRUE)[has_response] > 0
  )

  result
}

region_specs <- tibble::tibble(
  region_key = c(
    "head", "face", "chest", "abdomen",
    "upper_limb", "lower_limb", "back"
  ),
  region_label = c(
    "Head/scalp", "Face", "Chest", "Abdomen",
    "Upper limbs", "Lower limbs", "Back"
  ),
  site_vars = list(
    c("itch_head_scalp"),
    c("itch_face"),
    c("itch_chest"),
    c("itch_abdomen"),
    c(
      "itch_upper_arm", "itch_forearm",
      "itch_hand_fingers", "itch_palm"
    ),
    c(
      "itch_thigh", "itch_lower_leg",
      "itch_foot_top_toes", "itch_sole"
    ),
    c("itch_back")
  )
)

region_results <- purrr::map_dfr(seq_len(nrow(region_specs)), function(i) {
  flag <- get_any_site_s2(region_specs$site_vars[[i]])

  denominator <- sum(!is.na(flag))
  n_affected <- sum(flag == 1, na.rm = TRUE)

  tibble::tibble(
    region_key = region_specs$region_key[i],
    region_label = region_specs$region_label[i],
    n = n_affected,
    denominator = denominator,
    percentage = if (denominator > 0) {
      100 * n_affected / denominator
    } else {
      NA_real_
    }
  )
})

# ------------------------------------------------------------
# 5. Load and prepare bodymap polygons
# ------------------------------------------------------------

data("bodymap", package = "bodymap", envir = environment())

if (!inherits(bodymap, "sf")) {
  stop("The bodymap dataset is not an sf object; inspect bodymap package version.")
}

if (!all(c("id", "NAME") %in% names(bodymap))) {
  stop("The bodymap dataset does not contain the expected 'id' and 'NAME' columns.")
}

body_polygons <- bodymap %>%
  dplyr::mutate(
    view = dplyr::if_else(.data$id < 2000, "Anterior", "Posterior"),
    region_key = dplyr::case_when(
      .data$NAME == "Head" ~ "head",
      .data$NAME == "Face" ~ "face",
      .data$NAME == "Chest" ~ "chest",
      .data$NAME == "Belly" ~ "abdomen",
      .data$NAME == "Back" ~ "back",
      stringr::str_detect(.data$NAME, "Upper .* Limb") ~ "upper_limb",
      stringr::str_detect(.data$NAME, "Lower .* Limb") ~ "lower_limb",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::left_join(region_results, by = "region_key")

valid_region_percentages <- region_results$percentage[
  is.finite(region_results$percentage)
]

if (length(valid_region_percentages) == 0) {
  stop(
    "No broad anatomical regions could be calculated. ",
    "Check the questionnaire column names and response coding."
  )
}

mapped_max <- max(valid_region_percentages)
map_upper_limit <- max(40, ceiling(mapped_max / 10) * 10)

# ------------------------------------------------------------
# 6. FIGURE S2A: anterior/posterior anatomical map
# ------------------------------------------------------------

# Use actual polygon bounds to place headings above each silhouette.
view_labels <- body_polygons %>%
  dplyr::group_by(.data$view) %>%
  dplyr::summarise(
    geometry = sf::st_union(.data$geometry),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    bbox = purrr::map(.data$geometry, sf::st_bbox),
    label_x = purrr::map_dbl(
      .data$bbox,
      ~ (as.numeric(.x["xmin"]) + as.numeric(.x["xmax"])) / 2
    ),
    label_y = purrr::map_dbl(
      .data$bbox,
      ~ as.numeric(.x["ymax"]) + 5
    ),
    label = toupper(.data$view)
  )

map_bbox <- sf::st_bbox(body_polygons)
map_y_top <- max(view_labels$label_y) + 4

figure_S2A <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = body_polygons,
    ggplot2::aes(fill = .data$percentage),
    color = "white",
    linewidth = 0.30
  ) +
  # Keep the polygon geometry unchanged. Only reduce the ANTERIOR heading
  # so the two headings have more visual separation.
  ggplot2::geom_text(
    data = dplyr::filter(view_labels, .data$view == "Anterior"),
    ggplot2::aes(x = .data$label_x, y = .data$label_y, label = .data$label),
    inherit.aes = FALSE,
    size = 2.3,
    fontface = "bold",
    color = "#263746",
    vjust = 0
  ) +
  ggplot2::geom_text(
    data = dplyr::filter(view_labels, .data$view == "Posterior"),
    ggplot2::aes(x = .data$label_x, y = .data$label_y, label = .data$label),
    inherit.aes = FALSE,
    size = 3.1,
    fontface = "bold",
    color = "#263746",
    vjust = 0
  ) +
  ggplot2::scale_fill_viridis_c(
    option = "C",
    direction = 1,
    limits = c(0, map_upper_limit),
    breaks = scales::pretty_breaks(n = 5),
    oob = scales::squish,
    na.value = "grey90",
    name = "Patients reporting\nitch (%)"
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barheight = grid::unit(38, "mm"),
      barwidth = grid::unit(3.5, "mm")
    )
  ) +
  ggplot2::coord_sf(
    xlim = c(
      as.numeric(map_bbox["xmin"]) - 3,
      as.numeric(map_bbox["xmax"]) + 3
    ),
    ylim = c(
      as.numeric(map_bbox["ymin"]) - 3,
      map_y_top
    ),
    expand = FALSE,
    datum = NA
  ) +
  ggplot2::labs(
    x = NULL,
    y = NULL,
    caption = stringr::str_wrap(
      paste(
        "Conventional hemodialysis cohort (N =",
        nrow(data_hd_97),
        "). Values represent the percentage of patients reporting itch.",
        "Upper and lower limb regions represent any reported itch at the",
        "corresponding mapped sub-sites. Groin, buttocks, and clothing-contact",
        "areas are not mapped to non-equivalent anatomical polygons."
      ),
      width = 105
    )
  ) +
  ggplot2::theme_void(base_size = 9) +
  ggplot2::theme(
    legend.position = "right",
    legend.title = ggplot2::element_text(
      size = 8, face = "bold", color = "#263746"
    ),
    legend.text = ggplot2::element_text(size = 7.5, color = "grey25"),
    plot.caption = ggplot2::element_text(
      size = 7, color = "grey30", hjust = 0, lineheight = 1.2,
      margin = ggplot2::margin(t = 10)
    ),
    plot.caption.position = "plot",
    plot.margin = ggplot2::margin(t = 12, r = 12, b = 10, l = 12)
  )

print(figure_S2A)

ggplot2::ggsave(
  file.path(output_directory, "Supplementary_Figure_S2A_Anatomical_Map.tiff"),
  figure_S2A,
  width = 7, height = 7.5, units = "in",
  dpi = 600, compression = "lzw", bg = "white"
)

ggplot2::ggsave(
  file.path(output_directory, "Supplementary_Figure_S2A_Anatomical_Map.pdf"),
  figure_S2A,
  width = 7, height = 7.5, units = "in",
  device = grDevices::cairo_pdf, bg = "white"
)

# ------------------------------------------------------------
# 7. FIGURE S2B: detailed 16-location prevalence chart
# ------------------------------------------------------------

site_plot_data <- site_results %>%
  dplyr::filter(!is.na(.data$percentage), .data$denominator > 0) %>%
  dplyr::arrange(.data$percentage, .data$order) %>%
  dplyr::mutate(
    site = factor(.data$site, levels = .data$site),
    count_label = sprintf(
      "%d/%d (%.1f%%)",
      .data$n, .data$denominator, .data$percentage
    )
  )

if (nrow(site_plot_data) == 0) {
  stop(
    "No anatomical-site prevalence values are available for S2B. ",
    "Check the questionnaire columns and response coding."
  )
}

chart_max <- max(site_plot_data$percentage, na.rm = TRUE)
chart_right <- max(map_upper_limit, chart_max) + 17

figure_S2B <- ggplot2::ggplot(
  site_plot_data,
  ggplot2::aes(x = .data$percentage, y = .data$site)
) +
  ggplot2::geom_vline(
    xintercept = seq(0, ceiling(chart_max / 10) * 10, by = 10),
    color = "grey90",
    linewidth = 0.35
  ) +
  ggplot2::geom_col(
    ggplot2::aes(fill = .data$percentage),
    width = 0.62
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = .data$count_label),
    hjust = 0,
    nudge_x = 0.7,
    size = 2.7,
    color = "#263746"
  ) +
  ggplot2::scale_fill_viridis_c(
    option = "C",
    direction = 1,
    limits = c(0, map_upper_limit),
    oob = scales::squish,
    guide = "none"
  ) +
  ggplot2::scale_x_continuous(
    name = "Patients reporting itch (%)",
    limits = c(0, chart_right),
    breaks = scales::pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0))
  ) +
  ggplot2::scale_y_discrete(
    name = NULL,
    expand = expansion(add = 0.45)
  ) +
  ggplot2::labs(
    caption = paste(
      "Values are n/N (%). Denominators exclude missing responses",
      "at each location. Multiple anatomical locations could be",
      "reported by an individual patient."
    )
  ) +
  ggplot2::theme_classic(base_size = 9) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(
      size = 8, face = "bold", margin = ggplot2::margin(t = 8),
      color = "#263746"
    ),
    axis.text.x = ggplot2::element_text(size = 7.5, color = "grey30"),
    axis.text.y = ggplot2::element_text(size = 8, color = "#263746"),
    axis.line.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_line(color = "grey50", linewidth = 0.35),
    axis.ticks.x = ggplot2::element_line(color = "grey50", linewidth = 0.3),
    plot.caption = ggplot2::element_text(
      size = 7, color = "grey35", hjust = 0, lineheight = 1.2,
      margin = ggplot2::margin(t = 10)
    ),
    plot.caption.position = "plot",
    plot.margin = ggplot2::margin(t = 10, r = 18, b = 10, l = 10)
  )

print(figure_S2B)

ggplot2::ggsave(
  file.path(output_directory, "Supplementary_Figure_S2B_Site_Prevalence.tiff"),
  figure_S2B,
  width = 7, height = 6.5, units = "in",
  dpi = 600, compression = "lzw", bg = "white"
)

ggplot2::ggsave(
  file.path(output_directory, "Supplementary_Figure_S2B_Site_Prevalence.pdf"),
  figure_S2B,
  width = 7, height = 6.5, units = "in",
  device = grDevices::cairo_pdf, bg = "white"
)

message(
  "Supplementary Figures S2A and S2B exported to: ",
  normalizePath(output_directory)
)
