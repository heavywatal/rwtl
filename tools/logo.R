logo_plot()
logo_plot() +
  ggplot2::annotate("path", x = c(4, 4, 104, 104, 4), y = c(4, 104, 104, 4, 4)) +
  ggplot2::theme_bw() +
  erase(axis.title, axis.ticks, title)

logo_save("heavywatal.svg") |> logo_svg_optimize()
logo_save("heavywatal.png", height = 4) |> oxipng() |> cwebp(keep = TRUE)
logo_save("heavywatal-white.png", height = 4) |> oxipng() |> cwebp(keep = TRUE)
logo_save("heavywatal-circle.png", height = 4) |> oxipng() |> cwebp(keep = TRUE)
logo_save("heavywatal-circle-white.png", height = 4) |> oxipng() |> cwebp(keep = TRUE)
logo_save("heavywatal-circle-white-github.png", height = 1, dpi = 460) |> oxipng()
logo_save("heavywatal-circle-white-twitter.png", height = 1, dpi = 400) |> oxipng()
logo_save("heavywatal-white-garmin.png", height = 1, dpi = 400, expand = 0.3) |> oxipng()

logo_save_sticker("heavywatal-hex.png", height = 2.432) |>
  oxipng() |>
  fs::file_copy("../man/figures/logo.png", overwrite = TRUE)
logo_save_sticker("heavywatal-hex.svg") |>
  logo_svg_optimize("#202020") |>
  fs::file_copy("../man/figures/logo.svg", overwrite = TRUE)

mg = wtl::annotate_regpolygon(180L, radius = 61, x = 54, y = 54, fill = "#FFFFFF")
"heavywatal-tohoku_ac_jp-circle.png" |>
  logo_save(height = 4, midground = mg, bg = "#3E1485") |>
  oxipng() |>
  cwebp(keep = TRUE)

fs::dir_create("web")
logo_save("web/heavywatal.svg") |> logo_svg_optimize()
logo_save("web/favicon.png", 1, 32) |> oxipng() |> fs::file_move("web/favicon.ico")
logo_save("web/icon-192.png", 1, 192, expand = 0.125) |> oxipng()
logo_save("web/icon-512.png", 1, 512, expand = 0.125) |> oxipng()
logo_save("web/apple-touch-icon.png", 1, 180, expand = 0.125, bg = "#FFFFFF") |> oxipng()

fs::dir_create("test")
c(1, 2, 4, 6, 12) |>
  purrr::walk(\(height) {
    logo_save(wtl::glue("test/png-heavywatal-white-{height}.png"), height, dpi = 1200 / height)
    logo_save(wtl::glue("test/svg-heavywatal-white-{height}.svg"), height)
  })
