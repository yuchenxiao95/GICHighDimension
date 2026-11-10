# Runs the "Examples" code in README.md, writes the printed output back into
# each code block (as "#>" lines), redraws the figure that follows each block
# in man/figures/, and copies the code blocks with their output into
# Appendix 2 of vignettes/IChop.Rmd, which runs the same code (it
# defines set_seeds() earlier, in its Quick Start).
#
# Run from the package root:
#   Rscript data-raw/readme_figures.R
#
# Only the code from the "Examples" heading onward is run, so the installation
# code is skipped.

devtools::load_all(".", quiet = TRUE)
setup_julia()

# Compile the Julia search (vector and matrix response) and load glmnet before
# anything is timed, so the run times in the tables exclude one-off compilation.
local({
  set.seed(1)
  Xw <- matrix(rnorm(200 * 20), 200)
  yw <- Xw[, 1] + rnorm(200)
  for (Yw in list(yw, cbind(yw, yw + rnorm(200))))
    for (crit in c("Calculate_AIC", "Calculate_BIC", "Calculate_GIC6"))
      GICSelection(Xw, Yw, 1:5, crit, paste0(crit, "_short"), k = 20, Nsim = 1)
  for (Yw in list(yw, cbind(yw, yw + rnorm(200))))
    GICSelectionBoltzmann(Xw, Yw, 1:5, "Calculate_GIC6", "Calculate_GIC6_short", k = 20, Nsim = 1)
  invisible(glmnet::cv.glmnet(Xw, yw))
})

# Okabe-Ito colours, chosen to stay distinguishable (also for colour-blind readers)
palette <- c(AIC = "#0072B2", BIC = "#D55E00", GIC6 = "#009E73", Boltzmann = "#CC79A7")
methods <- c(palette, Lasso = "#E69F00", AdaLasso = "#000000")

# Lighter shade of a colour (used for n = 1000)
tint <- function(col) {
  rgb <- grDevices::col2rgb(col)
  grDevices::rgb(t(rgb + (255 - rgb) * 0.55), maxColorValue = 255)
}
size_lty <- c(n1000 = 1, n2000 = 3)       # solid: n = 1000, dotted: n = 2000

# Three panels: each criterion's value, and its model size, after every search
# step, with one line per criterion and sample size; then the run time of every
# method, including the lasso and adaptive lasso. Light grey vertical lines
# mark a new pass over the predictors; the dashed line marks the true size.
# `fits` is fits[[sample size]][[method]], as returned by compare_criteria().
# Signed log scale, sign(y) log10(1 + |y|), for criteria that go negative
slog <- function(y) sign(y) * log10(1 + abs(y))

plot_criteria <- function(fits, p, true_size, file, log_criterion = FALSE) {
  # 2 x 2 grid: criterion and model size on top, run time and the legend below
  png(file, width = 8, height = 6.7, units = "in", res = 250)
  on.exit(dev.off())
  layout(matrix(1:4, 2, byrow = TRUE))
  op <- par(mar = c(4.6, 5.2, 2.6, 1.2), las = 1,
            cex.axis = 1.25, cex.lab = 1.35, cex.main = 1.45)
  on.exit(par(op), add = TRUE)

  steps  <- seq_along(fits[[1]][[names(palette)[1]]]$GIC_values)
  passes <- seq(p, length(steps) - 1, by = p)
  # One column per (sample size, criterion)
  series <- function(f)
    do.call(cbind, lapply(fits, function(by_crit) sapply(by_crit[names(palette)], f)))
  values <- series(function(fit) fit$GIC_values)
  sizes  <- series(function(fit) lengths(fit$selected_coeffs))
  cols <- rep(palette, times = length(fits))
  ltys <- rep(size_lty[names(fits)], each = length(palette))

  # Criterion panel, optionally on the signed log scale, labelled in the
  # original units
  yv <- if (log_criterion) slog(values) else values
  matplot(steps, yv, type = "n", xlab = "Search step", ylab = "",
          yaxt = if (log_criterion) "n" else "s", main = "Criterion along the search")
  if (log_criterion) {
    r <- range(values)
    at <- c(-10^(4:0), 0, 10^(0:4))
    at <- at[at >= r[1] & at <= r[2]]
    if (length(at) < 3) at <- pretty(r)
    axis(2, at = slog(at), labels = format(at, big.mark = ",", trim = TRUE))
  }
  abline(v = passes, col = "grey85", lwd = 0.8)
  matlines(steps, yv, type = "s", lwd = 2.6, col = cols, lty = ltys)
  title(ylab = if (log_criterion) "Criterion value (signed log scale)" else "Criterion value",
        line = 3.9)

  # Log scale when the search starts from all predictors (linear and
  # multivariate examples), so the end of the path stays visible; both use the
  # same range and ticks, plus a tick at the true size
  log_ticks <- sort(unique(c(5, 10, 100, 500, true_size)))
  # Lasso and adaptive lasso: one model each, at the cross-validated lambda,
  # drawn as a horizontal line at its size
  shown <- methods[names(methods) %in% names(fits[[1]])]   # methods run in this example
  lasso <- setdiff(names(shown), names(palette))
  lasso_paths <- unlist(lapply(fits, function(by_method) by_method[lasso]), recursive = FALSE)
  lasso_y <- lapply(lasso_paths, function(fit) max(length(fit$selected), 1))
  log_y <- max(c(sizes, unlist(lasso_y)), na.rm = TRUE) > 100
  ylim <- range(c(sizes, unlist(lasso_y), true_size, if (log_y) range(log_ticks)), na.rm = TRUE)
  matplot(steps, sizes, type = "n", ylim = ylim,
          log = if (log_y) "y" else "", yaxt = if (log_y) "n" else "s",
          xlab = "Search step", ylab = "",
          main = sprintf("Model size (true size = %d)", true_size))
  if (log_y) axis(2, at = log_ticks)
  abline(v = passes, col = "grey85", lwd = 0.8)
  abline(h = true_size, lty = 2, col = "grey35")
  matlines(steps, sizes, type = "s", lwd = 2.6, col = cols, lty = ltys)
  lasso_col <- rep(methods[lasso], times = length(fits))
  lasso_lty <- rep(size_lty[names(fits)], each = length(lasso))
  for (i in seq_along(lasso_y))
    abline(h = lasso_y[[i]], lwd = 2.6, col = lasso_col[i], lty = lasso_lty[i])
  title(ylab = if (log_y) "Number of predictors (log scale)" else "Number of predictors",
        line = 3.9)

  # Run time of every method: light bars n = 1000, dark bars n = 2000
  secs <- sapply(fits, function(by_method)
    sapply(by_method[names(shown)], function(fit) fit$seconds))
  secs <- pmax(secs, 0.005)                 # keep sub-millisecond times on the log axis
  ylim_t <- 10^c(floor(log10(min(secs))), ceiling(log10(max(secs))))
  bars <- barplot(t(secs), beside = TRUE, log = "y", ylim = ylim_t, border = NA,
                  col = rbind(tint(shown), shown), names.arg = rep("", length(shown)),
                  main = "Run time", ylab = "")
  # method names on two staggered lines, so long names never overlap
  mtext(names(shown), side = 1, at = colMeans(bars), cex = 0.95,
        line = rep(c(0.6, 1.8), length.out = length(shown)))
  title(xlab = "light: n = 1000, dark: n = 2000", line = 3.4)
  title(ylab = "Seconds (log scale)", line = 3.9)
  box()

  # Legend in the fourth panel: method colours, then sample-size line types
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("center", inset = c(0, -0.12), legend = names(shown), col = shown,
         lty = 1, lwd = 3, bty = "n", cex = 1.25, seg.len = 3, ncol = 2,
         title = "Method", y.intersp = 1.3)
  legend("bottom", legend = c("n = 1000", "n = 2000"), col = "grey25",
         lty = size_lty, lwd = 2.5, horiz = TRUE, bty = "n", cex = 1.5,
         seg.len = 3, inset = c(0, 0.08))
  message("wrote ", file)
}

# Evaluate one code block in `env` and return its lines with fresh output.
run_block <- function(lines, env) {
  lines <- lines[!grepl("^#>", lines)]
  exprs <- parse(text = lines, keep.source = TRUE)
  refs  <- attr(exprs, "srcref")
  out   <- vector("list", length(lines))
  for (i in seq_along(exprs)) {
    printed <- utils::capture.output({
      res <- withVisible(eval(exprs[[i]], env))
      if (res$visible) print(res$value)
    })
    if (length(printed)) {
      last <- refs[[i]][3]
      out[[last]] <- c(out[[last]], paste("#>", printed))
    }
  }
  unlist(Map(c, lines, out), use.names = FALSE)
}

# Process one document: run its code blocks in order, refresh the output, and
# draw the figure named in the first image link after each block.
process <- function(path, start_heading, figure_dir, draw) {
  doc   <- readLines(path)
  start <- grep(start_heading, doc)[1]
  env   <- new.env(parent = globalenv())
  i <- start
  while (i <= length(doc)) {
    if (grepl("^```(r|\\{r\\})\\s*$", doc[i])) {
      end   <- i + which(grepl("^```\\s*$", doc[(i + 1):length(doc)]))[1]
      block <- run_block(doc[(i + 1):(end - 1)], env)
      doc   <- c(doc[seq_len(i)], block, doc[end:length(doc)])
      i     <- i + length(block) + 1

      nxt   <- i + 1
      while (nxt <= length(doc) && !grepl("^```", doc[nxt])) {
        m <- regmatches(doc[nxt], regexpr("README-[a-z-]+\\.png", doc[nxt]))
        if (length(m)) {
          if (draw) {
            truth <- if (!is.null(env$true_union) && grepl("multivariate", m))
              env$true_union else env$true_cols
            plot_criteria(env$fits, env$p, length(truth), file.path(figure_dir, m),
                          log_criterion = grepl("linear|multivariate", m))
          }
          break
        }
        nxt <- nxt + 1
      }
    }
    i <- i + 1
  }
  writeLines(doc, path)
  message("updated ", path)
}

# Copy the code blocks (with output) of the README examples into Appendix 2 of
# the article. The README is the source; the article's first block leaves out
# set_seeds(), which the article defines in its Quick Start.
sync_article <- function(readme, article) {
  closing <- function(doc, o) o + which(grepl("^```\\s*$", doc[(o + 1):length(doc)]))[1]
  rd <- readLines(readme)
  rs <- grep("^## Examples", rd)[1]
  r_open <- rs - 1 + which(grepl("^```r\\s*$", rd[rs:length(rd)]))
  art <- readLines(article)
  as <- grep("^### Helpers", art)[1]
  a_open <- as - 1 + which(grepl("^```\\{r\\}\\s*$", art[as:length(art)]))
  stopifnot(length(r_open) == length(a_open))
  out <- character(0)
  pos <- 1
  for (b in seq_along(a_open)) {
    new <- rd[(r_open[b] + 1):(closing(rd, r_open[b]) - 1)]
    a_close <- closing(art, a_open[b])
    old <- art[(a_open[b] + 1):(a_close - 1)]
    if (b == 1) {                      # drop the set_seeds() definition
      s0 <- grep("^set_seeds <- function", new)
      s1 <- s0 - 1 + which(new[s0:length(new)] == "}")[1]
      new <- new[-(s0:(s1 + (s1 < length(new) && new[s1 + 1] == "")))]
    }
    out <- c(out, art[pos:a_open[b]], new)
    pos <- a_close
  }
  writeLines(c(out, art[pos:length(art)]), article)
  message("updated ", article)
}

process("README.md", "^## Examples", "man/figures", draw = TRUE)
sync_article("README.md", "vignettes/IChop.Rmd")
