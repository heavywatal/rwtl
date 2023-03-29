#' Simple converter from list to TOML.
#'
#' @param data A list.
#' @rdname toml
#' @export
toTOML = function(data, usetz = TRUE) {
  v = data |>
    purrr::compact() |>
    purrr::keep(\(x) !is.na(x)) |>
    purrr::modify_if(is.character, \(x) paste0('"', x, '"')) |>
    purrr::modify_if(is_datetime, format_iso8601) |>
    purrr::modify_if(is.logical, tolower)
  paste0(names(v), " = ", v, collapse = "\n")
}

#' @description
#' `format_iso8601()` is compliant with TOML.
#' @inheritParams lubridate::format_ISO8601
#' @rdname toml
#' @export
format_iso8601 = function(x, usetz = TRUE, precision = NULL, ...) {
  lubridate::format_ISO8601(x, usetz = usetz, precision = precision, ...) |>
    stringr::str_replace("([-+]\\d\\d)(\\d\\d)$", "\\1:\\2")
}

is_datetime = function(x) {
  lubridate::is.Date(x) | lubridate::is.POSIXct(x)
}
