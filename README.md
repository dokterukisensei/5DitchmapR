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
- `plot_itch_tiles()` — compact tile summary.
- `itch_anatomy_regions()` — canonical IDs for the 16 distribution categories.
- `plot_itch_bodymap()` — schematic front/back regional map with hand/foot insets.
- `plot_itch_anatomy()` — maps prevalence onto anatomical polygons you supply, including front/back views.


## Detailed anatomical SVG template

DitchmapR includes an adapted front/back anatomical SVG template based on the
[Open Pain Illustration](https://github.com/DigitalHealthZurich/open-pain-illustration)
project. It includes translucent overlays for broad body locations relevant to
the 5-D Itch Scale.

```r
library(DitchmapR)
svg_path <- itch_anatomical_template()
browseURL(svg_path)
```

The overlay borders are **approximate schematic regions**, not validated lesion
boundaries or an official 5-D Itch Scale body diagram. In particular, clothing
or bandage contact is a questionnaire category rather than a discrete body
region. The source illustration was created for clinical pain documentation;
it has been adapted for itch-location visualization and this adaptation is not
endorsed by the original authors. See
[`inst/extdata/ANATOMICAL_ILLUSTRATION_ATTRIBUTION.md`](inst/extdata/ANATOMICAL_ILLUSTRATION_ATTRIBUTION.md)
for attribution, paper citation, and CC BY 4.0 terms.

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

## Anatomical body map

The package includes `plot_itch_bodymap()`, a built-in **schematic** front/back map with inset regions for palms, backs of hands/fingers, and soles. The 16 canonical categories follow the 5-D Itch Scale distribution checklist, including the clothing/bandage-contact item.

The map is intended for region-level epidemiological visualization, not precise lesion localization. The 5-D Itch Scale is a checklist of body locations; it does not define exact anatomical boundaries for every category. Accordingly, this map must not be described as a validated clinical anatomy map. Check the category labels against the questionnaire version and data dictionary used in your study.

```r
plot_itch_bodymap(demo)
```

For publication use, inspect the figure carefully and consider validating the region polygons with a dermatologist or clinical researcher familiar with the instrument. The clothing/bandage-contact category is displayed as an overlay band because it is a contact category rather than a body part.

## Citation

Cite the original 5-D Itch Scale paper and the package version used. Original instrument reference: Elman S, Hynan LS, Gabriel V, Mayo MJ. “The 5-D itch scale: a new measure of pruritus.” *British Journal of Dermatology*. 2010;162(3):587–593. doi:10.1111/j.1365-2133.2009.09564.x.

## Development status

Prototype / early development. The code and default labels require validation against the actual study data dictionary, testing with representative de-identified data, and an R package check before scientific or clinical use. Do not commit patient-level or identifiable clinical data to this public repository.
