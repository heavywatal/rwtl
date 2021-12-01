#' Helper functions to enhance ggridges package
#'
#' @details
#' [ridges2bars()] adds data points for [ggridges::geom_ridgeline()]
#' to make bar-shaped ridgelines.
#' @param .data data.frame
#' @param x,height Columns to transform
#' @param width Bar width.
#' @rdname ggridges-helper
#' @export
#' @examples
#' library(magrittr)
#' df_bars = tidyr::crossing(lambda = c(1, 2, 3), x = seq(0, 8)) %>%
#'   dplyr::mutate(height = dpois(x, lambda)) %>%
#'   ridges2bars(x, height) %>%
#'   print()
#' ggplot2::ggplot(df_bars) +
#'   ggplot2::aes(x, lambda) +
#'   ggridges::geom_ridgeline(ggplot2::aes(height = height, group = lambda))
#' ggplot2::ggplot(df_bars) +
#'   ggplot2::aes(lambda, x) +
#'   ggridges::geom_vridgeline(ggplot2::aes(width = height, group = lambda))
ridges2bars = function(.data, x, height, width = 0.9) {
  name_x = substitute(x)
  name_h = substitute(height)
  offset = 0.5 * width
  dplyr::mutate(.data,
    TMPx_1 = !!name_x - offset * (1 + 2e-8),
    TMPx_2 = !!name_x - offset * (1 + 1e-8),
    TMPx_3 = !!name_x - offset,
    TMPx_4 = !!name_x + offset,
    TMPx_5 = !!name_x + offset * (1 + 1e-8),
    TMPx_6 = !!name_x + offset * (1 + 2e-8),
    TMPh_1 = NA,
    TMPh_2 = 0,
    TMPh_3 = !!name_h,
    TMPh_4 = !!name_h,
    TMPh_5 = 0,
    TMPh_6 = NA,
    !!name_x := NULL,
    !!name_h := NULL
  ) %>%
    tidyr::pivot_longer(tidyselect::starts_with("TMP"), names_to = c(".value", NA), names_sep = "_") %>%
    dplyr::rename(!!name_x := .data$TMPx, !!name_h := .data$TMPh)
}
