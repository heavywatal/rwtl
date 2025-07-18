logo_plot()
logo_plot() +
  ggplot2::annotate("path", x = c(4, 4, 104, 104, 4), y = c(4, 104, 104, 4, 4)) +
  ggplot2::theme_bw() + erase(axis.title, axis.ticks, title)

logo_save("heavywatal.svg") |> logo_svg_optimize()
logo_save("heavywatal.png", height = 4) |> oxipng()
logo_save("heavywatal-white.png", height = 4) |> oxipng()
logo_save("heavywatal-circle.png", height = 4) |> oxipng()
logo_save("heavywatal-circle-white.png", height = 4) |> oxipng()
logo_save_sticker("heavywatal-hex.png", height = 2.432) |>
  oxipng() |>
  fs::file_copy("../man/figures/logo.png", overwrite = TRUE)
logo_save_sticker("heavywatal-hex.svg") |>
  logo_svg_optimize("#202020") |>
  fs::file_copy("../man/figures/logo.svg", overwrite = TRUE)

"heavywatal-tohoku_ac_jp-circle-white.png" |>
  logo_save(swoosh = "#3e1686", dot = "#3e1686") |>
  oxipng()

fs::dir_create("web")
logo_save("web/heavywatal.svg") |> logo_svg_optimize()
logo_save("web/favicon.png", 1, 32) |> oxipng() |> fs::file_move("web/favicon.ico")
logo_save("web/icon-192.png", 1, 192, expand = 0.125) |> oxipng()
logo_save("web/icon-512.png", 1, 512, expand = 0.125) |> oxipng()
logo_save("web/apple-touch-icon.png", 1, 180, expand = 0.125, bg = "#FFFFFF") |> oxipng()

ukraine_layers = function(x = c(-Inf, 54), y = 54) {
  list(
    geom_ribbon(aes(x = x, y = NULL, ymin = y, ymax = Inf), fill = "#0057b7"),
    geom_ribbon(aes(x = x, y = NULL, ymin = -Inf, ymax = y), fill = "#ffd700")
  )
}

mg = c(
  ukraine_layers(),
  wtl::annotate_regpolygon(180L, radius = 64, x = 54, y = 54, alpha = 0.66, fill = "#ffffff")
)
logo_save("heavywatal-ua-circle-white.png", midground = mg) |> oxipng()
logo_save("heavywatal-ua-white.png", midground = ukraine_layers()) |> oxipng()
logo_save("heavywatal-ua-circle-white-github.png", 1, 460, midground = mg) |> oxipng()
logo_save("heavywatal-ua-circle-white-twitter.png", 1, 400, midground = mg) |> oxipng()

logo_save("heavywatal-circle-white-github.png", 1, 460) |> oxipng()
logo_save("heavywatal-circle-white-twitter.png", 1, 400) |> oxipng()
logo_save("heavywatal-white-garmin.png", 2, 200, expand = 0.3) |> oxipng()

fs::dir_create("test")
c(1, 2, 4, 6, 12) |> purrr::walk(\(height) {
  logo_save(wtl::glue("test/png-heavywatal-white-{height}.png"), height, dpi = 1200 / height)
  logo_save(wtl::glue("test/svg-heavywatal-white-{height}.svg"), height)
})
