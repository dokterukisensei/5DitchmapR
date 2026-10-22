# Reproducible 5-D Itch Scale anatomical distribution from Excel
#
# Run in RStudio from the repository root after installing the package.
# IMPORTANT: edit the Excel path, sheet, and column mapping below. Never commit
# patient-level or identifiable clinical data to this public GitHub repository.
#
# Expected coding for each region: 1/0, TRUE/FALSE, or Yes/No (case-insensitive).
# Missing responses should remain NA and are excluded from that region's
# denominator by default.

# install.packages(c("readxl", "ggplot2", "pak"))
# pak::pak("dokterukisensei/5DitchmapR")

library(readxl)
library(DitchmapR)

# ---- 1. Input settings: EDIT THESE ----
excel_file <- file.choose()  # Select your de-identified working Excel file
sheet_name <- 1              # Or use the exact sheet name, e.g. "Data"
output_dir <- "itch_distribution_output"
denominator_method <- "available" # "available" or "all"

# Map package's canonical region IDs to the EXACT Excel column names.
# Replace the example names on the right with your actual spreadsheet headers.
region_columns <- c(
  head_scalp       = "itch_head_scalp",
  face             = "itch_face",
  chest            = "itch_chest",
  abdomen          = "itch_abdomen",
  back             = "itch_back",
  buttocks         = "itch_buttocks",
  thighs           = "itch_thighs",
  lower_legs       = "itch_lower_legs",
  feet_top         = "itch_top_of_feet",
  soles            = "itch_soles",
  palms            = "itch_palms",
  hands_back       = "itch_back_of_hands",
  forearms         = "itch_forearms",
  upper_arms       = "itch_upper_arms",
  clothing_contact = "itch_clothing_or_bandage_contact",
  groin            = "itch_groin"
)

# ---- 2. Read Excel and check column names ----
raw <- readxl::read_excel(excel_file, sheet = sheet_name)
names(raw) <- trimws(names(raw))

missing_columns <- setdiff(unname(region_columns), names(raw))
if (length(missing_columns)) {
  stop(
    "These mapped columns were not found in the Excel sheet: ",
    paste(missing_columns, collapse = ", "),
    "\nEdit region_columns to match names(raw) exactly.\nAvailable columns:\n",
    paste(names(raw), collapse = " | "),
    call. = FALSE
  )
}

# Keep only the mapped region fields; no participant identifiers are exported.
region_data <- as.data.frame(raw[, unname(region_columns), drop = FALSE])
names(region_data) <- names(region_columns)

# ---- 3. Normalize common binary response encodings ----
to_binary <- function(x, column_name) {
  if (is.logical(x)) return(as.numeric(x))

  if (is.numeric(x) || is.integer(x)) {
    bad <- !is.na(x) & !(x %in% c(0, 1))
    if (any(bad)) {
      stop("Column '", column_name,
           "' contains values other than 0, 1, or NA.", call. = FALSE)
    }
    return(as.numeric(x))
  }

  value <- tolower(trimws(as.character(x)))
  value[value %in% c("", "na", "n/a", "missing", "unknown")] <- NA_character_
  yes <- c("1", "yes", "y", "true", "affected", "present")
  no  <- c("0", "no", "n", "false", "not affected", "absent")
  bad <- !is.na(value) & !(value %in% c(yes, no))
  if (any(bad)) {
    stop("Unrecognized values in '", column_name, "': ",
         paste(unique(value[bad]), collapse = ", "),
         ". Update to_binary() to match the data dictionary.", call. = FALSE)
  }
  out <- rep(NA_real_, length(value))
  out[value %in% yes] <- 1
  out[value %in% no] <- 0
  out
}

for (nm in names(region_data)) {
  region_data[[nm]] <- to_binary(region_data[[nm]], nm)
}

if (nrow(region_data) == 0L) stop("No participant rows found.", call. = FALSE)

# ---- 4. Calculate counts and region-specific prevalence ----
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
prevalence <- DitchmapR::itch_prevalence(
  region_data,
  region_cols = names(region_columns),
  denominator = denominator_method
)
prevalence$region_id <- as.character(prevalence$region)
prevalence$region_label <- unname(DitchmapR::itch_anatomy_regions()[
  prevalence$region_id
])
prevalence$percent <- round(prevalence$percent, 1)

utils::write.csv(
  prevalence[, c("region_id", "region_label", "affected",
                 "denominator", "prevalence", "percent")],
  file.path(output_dir, "itch_region_prevalence.csv"), row.names = FALSE, na = ""
)

# Data columns are now renamed to canonical IDs; map each ID to itself.
canonical_map <- stats::setNames(names(region_columns), names(region_columns))

# ---- 5. Create figure outputs ----
# Detailed SVG template: best for editing in Illustrator/Inkscape.
svg_path <- DitchmapR::write_itch_bodymap_svg(
  region_data,
  region_cols = canonical_map,
  denominator = denominator_method,
  output_file = file.path(output_dir, "5d_itch_anatomical_distribution.svg")
)

# Schematic ggplot alternative, suitable for quick inspection in RStudio.
p <- DitchmapR::plot_itch_bodymap(
  region_data,
  region_cols = region_columns,
  denominator = denominator_method,
  title = "Itch distribution by body region"
)
ggplot2::ggsave(
  filename = file.path(output_dir, "5d_itch_anatomical_distribution.png"),
  plot = p, width = 12, height = 8, dpi = 600, bg = "white"
)
ggplot2::ggsave(
  filename = file.path(output_dir, "5d_itch_anatomical_distribution.pdf"),
  plot = p, width = 12, height = 8, bg = "white"
)

print(prevalence)
print(p)
message("Finished. Outputs saved to: ", normalizePath(output_dir))
message("SVG: ", normalizePath(svg_path))
