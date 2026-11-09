# Draws the IChop hex logo (man/figures/logo.png) and the pkgdown favicons.
#
# The logo is a Hopfield network on eight neurons: every pair is connected
# (faint edges), and the four selected neurons are orange and joined in
# orange. The name is set as "IC" (orange) + "hop" (white): information
# criterion + Hopfield.
#
# Run from the package root:  Rscript data-raw/logo.R  (needs ragg, base64enc)
# The favicon.ico is written by Python (Pillow) at the end, if available.

library(grid)

navy   <- "#182b4c"
orange <- "#ed9433"
faint  <- "#5a6b8c"

W <- 518; H <- 600

draw_logo <- function() {
  grid.newpage()
  pushViewport(viewport(xscale = c(0, W), yscale = c(H, 0)))

  # Hexagon (pointy top) with a thick orange rim and rounded corners.
  R  <- 248
  cx <- W / 2; cy <- H / 2
  a  <- pi / 2 + (0:5) * pi / 3
  grid.polygon(cx + R * cos(a), cy - R * sin(a), default.units = "native",
               gp = gpar(fill = navy, col = orange, lwd = 13, linejoin = "round"))

  # Eight neurons on an octagon, numbered clockwise from the top left; the
  # selected ones are top right, right lower, left lower and left upper.
  ncx <- W / 2; ncy <- 247; nr <- 113
  th  <- pi / 2 + pi / 8 - (0:7) * pi / 4
  nx  <- ncx + nr * cos(th); ny <- ncy - nr * sin(th)
  on  <- c(2, 4, 7, 8)
  pairs <- utils::combn(8, 2)
  for (k in seq_len(ncol(pairs))) {
    i <- pairs[1, k]; j <- pairs[2, k]
    grid.lines(nx[c(i, j)], ny[c(i, j)], default.units = "native",
               gp = gpar(col = faint, lwd = 1.1, alpha = 0.8))
  }
  sel <- utils::combn(on, 2)
  for (k in seq_len(ncol(sel))) {
    i <- sel[1, k]; j <- sel[2, k]
    grid.lines(nx[c(i, j)], ny[c(i, j)], default.units = "native",
               gp = gpar(col = orange, lwd = 3.2))
  }
  grid.circle(nx, ny, r = unit(17, "native"), default.units = "native",
              gp = gpar(fill = ifelse(seq_len(8) %in% on, orange, navy),
                        col = ifelse(seq_len(8) %in% on, orange, "white"),
                        lwd = 2.2))

  # "IC" in orange, "hop" in white, centred as one word.
  fs  <- gpar(fontsize = 68, fontface = "bold", fontfamily = "Helvetica")
  w1  <- convertWidth(grobWidth(textGrob("IC", gp = fs)), "native", TRUE)
  w2  <- convertWidth(grobWidth(textGrob("hop", gp = fs)), "native", TRUE)
  x0  <- W / 2 - (w1 + w2) / 2
  ty  <- 452
  grid.text("IC", x0, ty, just = c("left", "centre"), default.units = "native",
            gp = gpar(fontsize = 68, fontface = "bold", fontfamily = "Helvetica",
                      col = orange))
  grid.text("hop", x0 + w1, ty, just = c("left", "centre"), default.units = "native",
            gp = gpar(fontsize = 68, fontface = "bold", fontfamily = "Helvetica",
                      col = "white"))
  popViewport()
}

ragg::agg_png("man/figures/logo.png", width = W, height = H, units = "px", res = 72,
    bg = "transparent")
draw_logo()
invisible(dev.off())

# Favicons: the hex centred in a square, as before.
square <- function(file, size) {
  ragg::agg_png(file, width = size, height = size, units = "px", res = 72 * size / H,
      bg = "transparent")
  pushViewport(viewport(width = unit(W / H, "snpc"), height = unit(1, "snpc")))
  draw_logo()
  invisible(dev.off())
}
fav <- "pkgdown/favicon"
square(file.path(fav, "apple-touch-icon.png"), 180)
square(file.path(fav, "favicon-96x96.png"), 96)
square(file.path(fav, "web-app-manifest-192x192.png"), 192)
square(file.path(fav, "web-app-manifest-512x512.png"), 512)

# favicon.svg embeds the PNG logo, as the RealFaviconGenerator version did.
b64 <- base64enc::base64encode("man/figures/logo.png")
writeLines(sprintf(paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" ',
  'version="1.1" width="%d" height="%d" viewBox="0 0 %d %d">',
  '<image width="%d" height="%d" xlink:href="data:image/png;base64,%s"/></svg>'),
  W, H, W, H, W, H, b64), file.path(fav, "favicon.svg"))

# favicon.ico (48 and 32 px) via Pillow.
square(file.path(tempdir(), "ico48.png"), 48)
system2("python3", c("-I", "-c", shQuote(sprintf(
  "from PIL import Image; Image.open('%s').save('%s', sizes=[(48,48),(32,32)])",
  file.path(tempdir(), "ico48.png"), file.path(fav, "favicon.ico")))))
