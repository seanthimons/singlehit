## Draft hex logo: 1854 Soho streets and cholera deaths from John Snow's map
## behind an exponential dose-response curve that passes through the Broad Street
## pump. Renders one PNG per candidate palette; the palette is not chosen yet.
##
## The map data are the Snow.streets, Snow.pumps, and Snow.deaths tables from the
## HistData package, read here as CSV files, for example from
## https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Snow.streets.csv
## The wordmark needs the Open Sans font installed.
##
## Run from the package root:
## Rscript data-raw/logo.R <snow-csv-dir> <out-dir>

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: Rscript data-raw/logo.R <snow-csv-dir> <out-dir>")
}
snow_dir <- args[[1]]
out_dir <- args[[2]]
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

streets <- utils::read.csv(file.path(snow_dir, "Snow.streets.csv"))
pumps <- utils::read.csv(file.path(snow_dir, "Snow.pumps.csv"))
deaths <- utils::read.csv(file.path(snow_dir, "Snow.deaths.csv"))

hexagon <- function(radius) {
  angle <- (c(90, 150, 210, 270, 330, 30, 90)) * pi / 180
  sf::st_polygon(list(cbind(radius * cos(angle), radius * sin(angle))))
}
poly_df <- function(radius) as.data.frame(sf::st_coordinates(hexagon(radius)))
clip_path <- function(xy, radius) {
  line <- sf::st_intersection(sf::st_sfc(sf::st_linestring(xy)), hexagon(radius))
  as.data.frame(sf::st_coordinates(line))
}

inner_radius <- 0.925

## Map units per hex unit, and where the pump lands in hex coordinates.
map_scale <- 6.2
pump_at <- c(0.06, 0.15)
pump <- pumps[pumps$label == "Broad St", c("x", "y")]
to_hex <- function(x, y) {
  cbind((x - pump$x) / map_scale + pump_at[1], (y - pump$y) / map_scale + pump_at[2])
}

street_lines <- lapply(split(streets, streets$street), function(s) sf::st_linestring(to_hex(s$x, s$y)))
street_lines <- sf::st_intersection(sf::st_sfc(street_lines), hexagon(inner_radius))
street_lines <- sf::st_cast(street_lines[!sf::st_is_empty(street_lines)], "MULTILINESTRING")
street_xy <- as.data.frame(sf::st_coordinates(street_lines))
street_xy$id <- interaction(street_xy$L1, street_xy$L2)

death_xy <- as.data.frame(to_hex(deaths$x, deaths$y))
names(death_xy) <- c("x", "y")
keep <- sf::st_within(sf::st_as_sf(death_xy, coords = c("x", "y")), hexagon(0.9), sparse = FALSE)[, 1]
death_xy <- death_xy[keep, ]

## Exponential model on a log-dose axis, placed so the median infectious dose
## (P = 0.5) sits on the pump. `decade` is the hex width of one log10 unit.
decade <- 0.42
curve_height <- 0.94
u <- seq(-4, 4, length.out = 800)
p <- -expm1(-log(2) * 10^u)
curve_df <- clip_path(cbind(pump_at[1] + u * decade, pump_at[2] + (p - 0.5) * curve_height), 0.95)

## Fade the map out behind the wordmark.
fade_top <- seq(-0.32, -0.46, length.out = 10)
fade_df <- do.call(
  rbind,
  lapply(seq_along(fade_top), function(i) {
    band <- sf::st_polygon(list(cbind(c(-1, 1, 1, -1, -1), c(-1, -1, fade_top[i], fade_top[i], -1))))
    cbind(as.data.frame(sf::st_coordinates(sf::st_intersection(band, hexagon(inner_radius)))[, 1:2]), band = i)
  })
)

ring_df <- rbind(cbind(poly_df(1), part = 1), cbind(poly_df(inner_radius), part = 2))

systemfonts::register_variant("wordmark", "Open Sans", weight = "ultrabold")

draw_logo <- function(pal, file) {
  plot <- ggplot2::ggplot() +
    ggplot2::geom_polygon(ggplot2::aes(.data$X, .data$Y), data = poly_df(inner_radius), fill = pal$bg) +
    ggplot2::geom_path(
      ggplot2::aes(.data$X, .data$Y, group = .data$id),
      data = street_xy,
      colour = pal$streets,
      linewidth = 0.55,
      lineend = "round"
    ) +
    ggplot2::geom_point(ggplot2::aes(.data$x, .data$y), data = death_xy, colour = pal$cases, size = 0.5, alpha = 0.8) +
    ggplot2::geom_polygon(
      ggplot2::aes(.data$X, .data$Y, group = .data$band),
      data = fade_df,
      fill = pal$bg,
      alpha = 0.2
    ) +
    ggplot2::geom_path(ggplot2::aes(.data$X, .data$Y), data = curve_df, colour = pal$bg, linewidth = 6.2) +
    ggplot2::geom_path(ggplot2::aes(.data$X, .data$Y), data = curve_df, colour = pal$curve, linewidth = 3.4) +
    ggplot2::annotate("point", x = pump_at[1], y = pump_at[2], size = 17.5, colour = pal$bg) +
    ggplot2::annotate(
      "point",
      x = pump_at[1],
      y = pump_at[2],
      size = 14.5,
      shape = 21,
      stroke = 1.4,
      colour = pal$accent,
      fill = pal$bg
    ) +
    ggplot2::annotate("point", x = pump_at[1], y = pump_at[2], size = 8.2, colour = pal$accent) +
    ggplot2::geom_polygon(ggplot2::aes(.data$X, .data$Y, subgroup = .data$part), data = ring_df, fill = pal$border) +
    ggplot2::annotate(
      "text",
      x = 0,
      y = -0.565,
      label = "singlehit",
      family = "wordmark",
      size = 9.6,
      colour = pal$text
    ) +
    ggplot2::coord_fixed(xlim = c(-sqrt(3) / 2, sqrt(3) / 2), ylim = c(-1, 1), expand = FALSE) +
    ggplot2::theme_void()

  ragg::agg_png(file, width = 1039, height = 1200, res = 300, background = "transparent")
  print(plot)
  invisible(grDevices::dev.off())
}

palettes <- list(
  engraving = list(
    bg = "#F3EADB",
    border = "#1D3B45",
    streets = "#D6C8B0",
    cases = "#A8977C",
    curve = "#1F6F7F",
    accent = "#B13C2E",
    text = "#1D3B45"
  ),
  night = list(
    bg = "#102A33",
    border = "#287D8E",
    streets = "#24505C",
    cases = "#3F8090",
    curve = "#F4E1D2",
    accent = "#F2A03D",
    text = "#F4E1D2"
  ),
  thames = list(
    bg = "#0B4F7A",
    border = "#06324E",
    streets = "#2B74A3",
    cases = "#5C9CC4",
    curve = "#FFFFFF",
    accent = "#F28C28",
    text = "#FFFFFF"
  ),
  viridis = list(
    bg = "#2D1650",
    border = "#1B0C33",
    streets = "#4B2F78",
    cases = "#7556A6",
    curve = "#FDE725",
    accent = "#35B779",
    text = "#FDE725"
  )
)

for (name in names(palettes)) {
  draw_logo(palettes[[name]], file.path(out_dir, paste0("logo-", name, ".png")))
}
