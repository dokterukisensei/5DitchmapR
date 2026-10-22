# Data-driven anatomical itch map (custom vector anatomy)
# Repository: dokterukisensei/5DitchmapR
# Input: Excel questionnaire with Japanese headers; one row per participant.
# This script draws a custom front/back human-shaped vector map using ggplot2.
# It does not use or upload patient identifiers.
#
# Run in RStudio. Packages: readxl, ggplot2, dplyr, tidyr, scales, svglite (optional)

required <- c("readxl", "ggplot2", "dplyr", "tidyr", "scales")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

# ---- 1. Import and validate questionnaire data -----------------------------
message("Select CSV body itch.xls")
input_file <- file.choose()
raw <- as.data.frame(read_excel(input_file, sheet = 1))
n <- nrow(raw)
if (n < 1) stop("No participant rows found.")

# Map Excel's Japanese region headers to stable analysis IDs.
region_cols <- c(
  head_scalp = "頭/頭皮",
  face = "顔",
  chest = "胸",
  abdomen = "腹部",
  back = "背中",
  buttocks = "おしり",
  thighs = "ふともも",
  lower_legs = "すね",
  feet_top_toes = "足の甲/あしゆび",
  soles = "足の裏",
  palms = "手のひら",
  hands_fingers = "手の甲/手指",
  forearms = "前腕（肘から下の部分の腕）",
  upper_arms = "上腕（肘から肩の部分の腕）",
  clothing_contact = "服の触れる部位",
  groin = "股"
)
missing_cols <- setdiff(unname(region_cols), names(raw))
if (length(missing_cols)) {
  stop("Missing Excel columns: ", paste(missing_cols, collapse = ", "))
}
for (nm in unname(region_cols)) {
  z <- suppressWarnings(as.numeric(as.character(raw[[nm]])))
  if (anyNA(z) || !all(z %in% c(0, 1))) {
    stop("Column '", nm, "' must contain only complete 0/1 values.")
  }
  raw[[nm]] <- z
}

prev <- tibble(
  region = names(region_cols),
  n_itch = vapply(names(region_cols), function(id) sum(raw[[region_cols[[id]]]] == 1), numeric(1)),
  denominator = n
) |>
  mutate(prevalence = 100 * n_itch / denominator)

# ---- 2. Draw a vector human template ---------------------------------------
# Coordinates are schematic illustration coordinates, not clinical lesion boundaries.
# Regions are separate polygons so their fills are data-driven.
poly <- function(region, view, x, y) {
  tibble(region = region, view = view, x = x, y = y)
}
P <- list()
add <- function(region, view, x, y) {
  P[[length(P) + 1L]] <<- poly(region, view, x, y)
}
# Head, each view uses a rounded polygon.
ellipse <- function(cx, cy, rx, ry, n = 36) {
  a <- seq(0, 2*pi, length.out = n)
  tibble(x = cx + rx*cos(a), y = cy + ry*sin(a))
}
add("head_scalp", "Back", ellipse(0, 8.45, .34, .48)$x, ellipse(0, 8.45, .34, .48)$y)
add("head_scalp", "Front", ellipse(4, 8.45, .34, .48)$x, ellipse(4, 8.45, .34, .48)$y)

# Back torso: shoulders, upper back, abdomen/flank area, buttocks.
add("back", "Back", c(-.48,-.68,-.62,-.53,-.48,-.44,-.40,-.37,-.32,.32,.37,.40,.44,.48,.53,.62,.68,.48,.36,0,-.36,-.48),
    c(7.65,7.58,7.25,6.85,6.50,6.15,5.95,5.88,5.82,5.82,5.88,5.95,6.15,6.50,6.85,7.25,7.58,7.65,7.78,7.82,7.78,7.65))
add("abdomen", "Back", c(-.32,-.36,-.31,-.27,0,.27,.31,.36,.32,.25,0,-.25),
    c(5.82,5.40,5.05,4.92,4.86,4.92,5.05,5.40,5.82,5.88,5.90,5.88))
add("buttocks", "Back", c(-.30,-.34,-.31,-.24,0,.24,.31,.34,.30,.18,0,-.18),
    c(4.92,4.58,4.36,4.25,4.22,4.25,4.36,4.58,4.92,4.98,4.96,4.98))

# Front torso split into chest and abdomen.
add("chest", "Front", c(3.52,3.42,3.48,3.56,3.78,4.00,4.22,4.44,4.52,4.48,4.40,4.32,4.00,3.68,3.60),
    c(7.65,7.52,7.12,6.72,6.58,6.56,6.58,6.72,7.12,7.52,7.65,7.76,7.80,7.76,7.65))
add("abdomen", "Front", c(3.68,3.60,3.64,3.72,4.00,4.28,4.36,4.40,4.32,4.20,4.00,3.80),
    c(6.58,6.15,5.65,5.28,5.18,5.28,5.65,6.15,6.58,6.52,6.50,6.52))
add("groin", "Front", c(3.72,3.80,4.00,4.20,4.28,4.18,4.00,3.82),
    c(5.28,4.92,4.82,4.92,5.28,5.02,4.96,5.02))
# Upper arms and forearms (both sides), with a gentle tapered shape.
for (offset in c(0, 4)) {
  # left/right upper arm on each body; same region, distinct polygons
  for (sgn in c(-1, 1)) {
    cx <- offset + sgn*.62
    add("upper_arms", if (offset == 0) "Back" else "Front",
        cx + sgn*c(-.12,-.20,-.21,-.17,-.10,.00,.06,.02,-.04),
        c(7.60,7.45,7.05,6.70,6.55,6.66,7.05,7.40,7.58))
    cx2 <- offset + sgn*.76
    add("forearms", if (offset == 0) "Back" else "Front",
        cx2 + sgn*c(-.08,-.15,-.17,-.13,-.06,.03,.08,.05,-.02),
        c(6.48,6.20,5.78,5.42,5.35,5.48,5.88,6.20,6.43))
  }
}
# Thighs, lower legs, feet; left and right legs for front and back.
for (offset in c(0, 4)) {
  view <- if (offset == 0) "Back" else "Front"
  for (sgn in c(-1, 1)) {
    cx <- offset + sgn*.20
    add("thighs", view,
        cx + sgn*c(-.16,-.20,-.20,-.17,-.13,-.10,.00,.08,.13,.17,.20,.20,.16),
        c(4.28,4.00,3.55,3.05,2.72,2.58,2.55,2.62,2.82,3.20,3.65,4.00,4.28))
    cx2 <- offset + sgn*.19
    add("lower_legs", view,
        cx2 + sgn*c(-.10,-.13,-.12,-.08,-.06,-.04,.00,.05,.07,.09,.11,.12,.10),
        c(2.58,2.28,1.88,1.45,.98,.60,.48,.60,.98,1.45,1.88,2.28,2.58))
    add("feet_top_toes", view,
        c(offset+sgn*.10, offset+sgn*.12, offset+sgn*.16, offset+sgn*.20, offset+sgn*.23, offset+sgn*.20, offset+sgn*.12),
        c(.60,.38,.27,.22,.12,.05,.08))
  }
}
# Hands are shown at the ends of arms; both palms and dorsal hands are schematic.
for (offset in c(0, 4)) {
  view <- if (offset == 0) "Back" else "Front"
  for (sgn in c(-1, 1)) {
    hx <- offset + sgn*.84
    add("hands_fingers", view,
        hx + c(-.07,-.08,-.06,-.03,.00,.04,.07,.08,.06,.02,-.02,-.05),
        c(5.42,5.25,5.06,4.98,5.02,5.00,5.08,5.28,5.43,5.48,5.47,5.44))
  }
}
# Soles are small lower-foot overlays; kept distinct for questionnaire category.
for (offset in c(0, 4)) {
  for (sgn in c(-1, 1)) {
    add("soles", if (offset == 0) "Back" else "Front",
        c(offset+sgn*.12, offset+sgn*.16, offset+sgn*.21, offset+sgn*.23, offset+sgn*.18),
        c(.18,.10,.10,.03,.02))
  }
}
shapes <- bind_rows(P)

# Region values; clothing contact is not a unique anatomical region, so report separately.
shape_values <- prev |> filter(region != "clothing_contact")
shapes <- shapes |> left_join(shape_values, by = "region")

# ---- 3. Render and export ---------------------------------------------------
output_dir <- file.path(dirname(input_file), "itch_distribution_output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Add small region key on the right so text never covers the figure.
key <- prev |>
  mutate(label = sprintf("%s  %d/%d (%.1f%%)",
                         recode(region,
                           head_scalp = "Head/scalp", face = "Face",
                           chest = "Chest", abdomen = "Abdomen",
                           back = "Back", buttocks = "Buttocks",
                           thighs = "Thighs", lower_legs = "Lower legs",
                           feet_top_toes = "Top of feet/toes", soles = "Soles",
                           palms = "Palms", hands_fingers = "Hands/fingers",
                           forearms = "Forearms", upper_arms = "Upper arms",
                           clothing_contact = "Clothing contact", groin = "Groin"),
                         n_itch, denominator, prevalence))

# Face, palms and clothing-contact are listed in the key even when no distinct
# surface is drawn in the simplified anatomical template.
p <- ggplot() +
  geom_polygon(
    data = shapes,
    aes(x = x, y = y, group = interaction(view, region, row_number()),
        fill = prevalence),
    colour = "white", linewidth = 0.25
  ) +
  facet_wrap(~view, nrow = 1) +
  scale_fill_viridis_c(
    option = "C", limits = c(0, max(prev$prevalence)),
    breaks = pretty(c(0, max(prev$prevalence)), n = 5),
    labels = function(x) paste0(round(x), "%"),
    name = "Participants\nreporting itch",
    na.value = "grey90"
  ) +
  coord_equal(ylim = c(0, 9.1), xlim = c(-1.2, 5.2), expand = FALSE) +
  labs(
    title = sprintf("Anatomical Distribution of Pruritus (N = %d)", n),
    subtitle = "Schematic regional map; boundaries are illustrative, not clinical lesion borders",
    caption = sprintf(
      "Percentages use all %d participants. Regions are multiple-response categories. Clothing contact is reported separately.",
      n
    ),
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 12) +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    plot.title = element_text(size = 17, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 10, hjust = .5),
    plot.caption = element_text(size = 9, hjust = 0),
    legend.position = "right",
    panel.spacing = grid::unit(1.2, "cm"),
    plot.margin = margin(12, 15, 12, 15)
  )

print(p)

ggsave(file.path(output_dir, "anatomical_bodymap_vector.png"),
       p, width = 11, height = 8, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "anatomical_bodymap_vector.pdf"),
       p, width = 11, height = 8, device = "pdf", bg = "white")
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(file.path(output_dir, "anatomical_bodymap_vector.svg"),
         p, width = 11, height = 8, device = svglite::svglite, bg = "white")
}

# Export only aggregate counts, never the patient-level source data.
write.csv(prev, file.path(output_dir, "region_prevalence.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# Report the clothing-contact result separately because it is an overlay category.
contact <- prev |> filter(region == "clothing_contact")
cat("\nClothing-contact category: ",
    contact$n_itch, "/", contact$denominator, " (",
    sprintf("%.1f", contact$prevalence), "%)\n", sep = "")
cat("Saved figures and aggregate table in: ", output_dir, "\n", sep = "")
