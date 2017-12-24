library(tidyverse)

plot_logo = function(circle_scale=1.0, padding=10) {
  .path = tibble(
    x = c(35, 11, 55, 41, 75, 71, 95),
    y = c(95, 5, 73, 5, 51, 5, 29)
  )
  .poly = tibble(
    x = c(1, 6, 99),
    y = c(35, 22, 72)
  )
  .dot = tibble(x = 77, y = 77)
  .limits = c(-padding, 100 + padding)
  ggplot(.path, aes(x, y)) +
    geom_polygon(data = .poly, fill = "#e08010") +
    geom_path(size = 6, linejoin = "mitre") +
    geom_point(size = 18 * circle_scale, data = .dot, colour = "#a4321a") +
    coord_fixed(xlim = .limits, ylim = .limits, expand = FALSE) +
    theme_void()
}

dev.off()
quartz(width = 4, height = 4)
plot_logo()

ggsave("heavywatal.png", plot_logo(), height = 4, width = 4, dpi = 300, bg = "transparent")
ggsave("heavywatal.svg", plot_logo(0.75), height = 4, width = 4, bg = "transparent")
# svglite bug?
