# 5DitchmapR

**An R package starter for visualizing body-region itch distribution in clinical research.**

The repository is named **5DitchmapR**. The internal R package name is **DitchmapR** because valid R package names must begin with a letter.

`DitchmapR` validates binary region data and creates reproducible plots of region-specific itch prevalence for the 16 distribution choices associated with the 5-D Itch Scale.

> **Scope and permissions:** This is an independent, unofficial visualization utility. It does not provide the questionnaire, reproduce validated scoring rules, or replace the approved instrument. Verify the exact region labels against the questionnaire version and data dictionary used in your study, and check applicable permissions before redistribution.

## Current functions

- `itch_regions()` — default display labels for 16 region columns.
- `validate_itch_data()` — checks required columns and binary values.
- `itch_prevalence()` — computes region-specific affected counts and percentages.
- `plot_itch_prevalence()` — horizontal prevalence bar chart.
- `plot_itch_tiles()` — compact tile summary. **This is not an anatomical body silhouette.**
- `plot_itch_anatomy()` — maps prevalence onto anatomical polygons you supply, including front/back views.

## Install

Once the repository is public:

```r
install.packages("pak")
pak::pak("dokterukisensei/5DitchmapR")
library(DitchmapR)
```

For local development, clone or download this repository, open the package directory in R, and run:

```r
install.packages(c("devtools", "testthat", "ggplot2", "scales"))
devtools::load_all()
devtools::test()
devtools::check()
```

## Quick start

One row per participant; each region column should be coded `1` for affected, `0` for not affected, and `NA` if unknown. Replace the demo data with a de-identified data frame and confirm the labels and coding against your data dictionary.

```r
library(DitchmapR)
regions <- itch_regions()

# Demo data only; not real patient data
set.seed(2026)
demo <- as.data.frame(replicate(length(regions), rbinom(80, 1, .25)))
names(demo) <- regions

validate_itch_data(demo, regions)
itch_prevalence(demo, regions, denominator = "available")
plot_itch_prevalence(demo, regions)
plot_itch_tiles(demo, regions)
```

## Missing values

- `denominator = "available"` (default): denominator is non-missing responses for each region.
- `denominator = "all"`: denominator is all rows; missing values are not counted as affected.

Choose the option that matches the data collection and analysis plan, and report the denominator in figure notes or the manuscript.

## Anatomical map

The package now includes `plot_itch_anatomy()`, which colors supplied anatomical polygons by regional itch prevalence. The function does **not** bundle a human-body silhouette: anatomical outlines may have licensing restrictions, and arbitrary coordinates could misrepresent the questionnaire regions.

Your polygon data frame must contain:

- `region`: exact region-column name from `region_cols`
- `x`, `y`: numeric polygon coordinates
- optional `polygon_id`: ID for separate polygon pieces
- optional `view`: e.g. `Front` and `Back`, shown in separate panels

Example (replace `body_polygons` with a licensed, correctly mapped polygon dataset):

```r
plot_itch_anatomy(
  data = demo,
  polygons = body_polygons,
  region_cols = regions
)
```

Each polygon is colored by the percentage reporting itch in its linked region. Regions should be matched to the actual questionnaire/data dictionary; do not imply anatomical precision beyond the data collected. The tile chart remains a schematic summary, not an anatomical body map.

## Citation

Cite the original 5-D Itch Scale paper and the package version used. Original instrument reference: Elman S, Hynan LS, Gabriel V, Mayo MJ. “The 5-D itch scale: a new measure of pruritus.” *British Journal of Dermatology*. 2010;162(3):587–593. doi:10.1111/j.1365-2133.2009.09564.x.

## Development status

Prototype / early development. The code and default labels require validation against the actual study data dictionary, testing with representative de-identified data, and an R package check before scientific or clinical use. Do not commit patient-level or identifiable clinical data to this public repository.
