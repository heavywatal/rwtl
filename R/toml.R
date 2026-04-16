#' Simple converter from list to TOML.
#'
#' @param data A list.
#' @rdname toml
#' @export
toTOML = function(data, usetz = TRUE) {
  v = data |>
    purrr::compact() |>
    purrr::discard(is.list) |>
    purrr::discard(is.na) |>
    purrr::modify_if(is.character, \(x) paste0('"', x, '"')) |>
    purrr::modify_if(is_datetime, \(x) format_iso8601(x, usetz = usetz)) |>
    purrr::modify_if(is.logical, tolower)
  res = stringr::str_c(names(v), " = ", v, collapse = "\n")
  l = data |>
    purrr::keep(is.list) |>
    purrr::modify(\(x) toTOML(x, usetz = usetz))
  if (length(l) > 0L) {
    tables = stringr::str_c("[", names(l), "]\n", l, collapse = "\n")
    res = stringr::str_c(res, tables, sep = "\n")
  }
  res
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
