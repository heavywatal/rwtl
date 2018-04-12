#' Set aliases for my favorite fonts
#' @rdname font
#' @export
set_quartz_font = function() {
  .styles = c("", " Bold", " Italic", " Bold Italic")
  grDevices::quartzFonts(
    serif = grDevices::quartzFont(
      paste0("Noto Serif", .styles)
    ),
    sans = grDevices::quartzFont(
      paste0("Source Sans Pro", .styles)
    ),
    mono = grDevices::quartzFont(
      paste0("Ubuntu Mono", .styles)
    ),
    mincho = grDevices::quartzFont(
      paste0("Hiragino Mincho ProN W", c(3, 6, 3, 6))
    ),
    gothic = grDevices::quartzFont(
      paste0("Hiragino Kaku Gothic ProN W", c(3, 6, 3, 6))
    )
  )
}

#' @rdname font
#' @export
set_pdf_fonts = function() {
  grDevices::pdfFonts(
    serif = grDevices::pdfFonts()$Palatino,
    mincho = grDevices::pdfFonts()$Japan1,
    gothic = grDevices::pdfFonts()$Japan1GothicBBB
  )
  if (is.null(extrafont::fonts())) {
    return(invisible())
  }
  tryCatch({
    grDevices::pdfFonts(
      sans = grDevices::pdfFonts()$`Source Sans Pro`,
      mono = grDevices::pdfFonts()$`Ubuntu Mono`
    )
  }, error = warning)
}
