#' Locate the bundled anatomical SVG template
#'
#' Returns the path to the adapted front/back anatomical SVG included with
#' DitchmapR. The template includes approximate translucent overlays for broad
#' 5-D Itch Scale locations. These overlays are schematic and are not validated
#' clinical boundaries.
#'
#' The source illustration is licensed CC BY 4.0. See the bundled attribution
#' file at `inst/extdata/ANATOMICAL_ILLUSTRATION_ATTRIBUTION.md`.
#'
#' @return A single file path to the SVG template, or an error if the file is
#'   not available in the installed package.
#' @export
itch_anatomical_template <- function() {
  path <- system.file("extdata", "5d_itch_bodymap.svg", package = "DitchmapR")
  if (!nzchar(path) || !file.exists(path)) {
    stop("The bundled SVG template was not found. Reinstall DitchmapR.", call. = FALSE)
  }
  path
}
