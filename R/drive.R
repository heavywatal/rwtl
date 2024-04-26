#' Read spreadsheet from cloud drives
#'
#' @inheritDotParams googlesheets4::read_sheet
#' @rdname drive
#' @export
gs4_read_sheet = function(..., path = NULL, email = gargle::gargle_oauth_email()) {
  if (is.null(googledrive::drive_oauth_client())) {
    gs4_auth_json(path, email)
  }
  googlesheets4::read_sheet(...)
}

#' @param path Credentials JSON downloaded from "Google Cloud Console".
#' Passed to [googledrive::drive_auth_configure()].
#' @param email An address or a glob pattern like `"*@tohoku.ac.jp"`.
#' `TRUE` to allow auto-discovery from cache.
#' Passed to [googledrive::drive_auth()]
#' @rdname drive
#' @export
gs4_auth_json = function(path = NULL, email = gargle::gargle_oauth_email()) {
  drive_auth_json(path, email)
  googlesheets4::gs4_auth(token = googledrive::drive_token())
}

drive_auth_json = function(path, email) {
  googledrive::drive_auth_configure(path = path %||% Sys.getenv("GOOGLE_OAUTH_JSON"))
  googledrive::drive_auth(email = email)
}
