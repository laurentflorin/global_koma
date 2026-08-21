# Stage 3: regional and global aggregation blocks.
#
# Post-estimation, country-level forecasts are rolled up into regional
# (euro-area) and global blocks for reporting -- distinct from the stage-2
# ea_/world_ *identities*, which are enforced during estimation. Stage 3
# blocks can also mix in countries that were not part of stage 2 (e.g.
# smaller economies tracked only for reporting, not jointly estimated).

#' Define regional block membership
#'
#' @param block_map A named list, block name -> character vector of ISO-2
#'   member country codes (e.g. `list(ea = c("de","fr","it","es"))`).
#'
#' @return `block_map`, validated: every block name is a valid
#'   [shared_var()] scope and every member is a two-letter lowercase code.
#' @export
define_blocks <- function(block_map) {
  stop("not implemented", call. = FALSE)
}

#' Build one block's aggregation identity
#'
#' @param block Block name, e.g. `"ea"`.
#' @param concept Concept to aggregate, e.g. `"gdp"`.
#' @param members Character vector of ISO-2 member codes.
#' @param weights Named numeric vector, one weight per `members` (see
#'   [country_weights()]).
#'
#' @return A single equation string (see [weighted_identity()]).
#' @export
stage3_block_identity <- function(block, concept, members, weights) {
  stop("not implemented", call. = FALSE)
}

#' Aggregate a fitted or forecast panel into regional/global blocks
#'
#' Applies every block's weights (post-estimation, not as an estimated
#' identity) to roll country-level `koma_forecast` or `koma_estimate`
#' output up into block-level series for reporting.
#'
#' @param panel Named list of `koma_ts` (forecasts or fitted values, keyed
#'   by `<iso2>_<concept>`).
#' @param block_map As returned by [define_blocks()].
#' @param concepts Character vector of concepts to aggregate for every
#'   block.
#' @param weights Named list of [country_weights()] vectors, one per
#'   block.
#'
#' @return Named list of `koma_ts`, the block-level aggregates.
#' @export
aggregate_blocks <- function(panel, block_map, concepts, weights) {
  stop("not implemented", call. = FALSE)
}
