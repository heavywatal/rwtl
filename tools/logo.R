plot_logo()
plot_logo() + ggplot2::theme_bw() + erase(axis.title, axis.ticks, title)

save_logo("heavywatal.svg")
save_logo("heavywatal.png") |> oxipng()
save_logo("heavywatal-white.png") |> oxipng()
save_logo("heavywatal-circle.png") |> oxipng()
save_logo("heavywatal-circle-white.png") |> oxipng()
save_sticker("heavywatal-hex.png") |>
  oxipng() |>
  fs::file_copy("../man/figures/logo.png", overwrite = TRUE)
save_sticker("heavywatal-hex.svg") |>
  fs::file_copy("../man/figures/logo.svg", overwrite = TRUE)

"heavywatal-tohoku_ac_jp-circle-white.png" |>
  save_logo(swoosh = "#3e1686", dot = "#3e1686") |>
  oxipng()

fs::dir_create("web")
save_logo("web/favicon.svg")
save_logo("web/favicon.png", 1, 32) |> oxipng()
save_logo("web/icon-192.png", 1, 192) |> oxipng()
save_logo("web/icon-512.png", 1, 512) |> oxipng()
save_logo("web/apple-touch-icon.png", 1, 180, bg = "#ffffff") |> oxipng()

ukraine_layers = function(x = c(-Inf, 0.5), y = 0.5) {
  list(
    geom_ribbon(aes(x = x, y = NULL, ymin = y, ymax = Inf), fill = "#0057b7"),
    geom_ribbon(aes(x = x, y = NULL, ymin = -Inf, ymax = y), fill = "#ffd700")
  )
}

mg = c(
  ukraine_layers(),
  wtl::annotate_regpolygon(180L, radius = 0.60, x = 0.5, y = 0.5, alpha = 0.66, fill = "#ffffff")
)
save_logo("heavywatal-ua-circle-white.png", midground = mg) |> oxipng()
save_logo("heavywatal-ua-white.png", midground = ukraine_layers()) |> oxipng()

fs::dir_create("test")
c(1, 2, 4, 6, 12) |> purrr::walk(\(height) {
  save_logo(wtl::glue("test/png-heavywatal-white-{height}.png"), height, dpi = 1200 / height)
  save_logo(wtl::glue("test/svg-heavywatal-white-{height}.svg"), height)
})
