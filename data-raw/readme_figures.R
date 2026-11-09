# Runs the "Examples" code in README.md and in vignettes/examples.Rmd,
# writes the printed output back into each code block (as "#>" lines), and
# redraws the figure that follows each block in man/figures/.
#
# Run from the package root:
#   Rscript data-raw/readme_figures.R
#
# Only the code from the "Examples" heading (README) or the "Helpers" heading
# (article) onward is run, so the installation code is skipped.

devtools::load_all(".", quiet = TRUE)
setup_julia()

palette <- c(AIC = "#2a78d6", BIC = "#eb6834", GIC6 = "#1baf7a")
size_lty <- c(n1000 = 1, n2000 = 3)       # solid: n = 1000, dotted: n = 2000

# Two panels: each criterion's value, and its model size, after every search
# step, with one line per criterion and sample size. Light grey vertical lines
# mark a new pass over the predictors; the dashed line marks the true size.
# `fits` is fits[[sample size]][[criterion]], as returned by compare_criteria().
plot_criteria <- function(fits, p, true_size, file) {
  png(file, width = 8, height = 3.9, units = "in", res = 200)
  on.exit(dev.off())
  op <- par(mfrow = c(1, 2), mar = c(4.2, 4.6, 2.2, 1), oma = c(2.6, 0, 0, 0),
            las = 1, cex.axis = 0.95, cex.lab = 1.1, cex.main = 1.1)
  on.exit(par(op), add = TRUE)

  steps  <- seq_along(fits[[1]][[names(palette)[1]]]$GIC_values)
  passes <- seq(p, length(steps) - 1, by = p)
  # One column per (sample size, criterion); the lasso fits have no search path
  series <- function(f)
    do.call(cbind, lapply(fits, function(by_crit) sapply(by_crit[names(palette)], f)))
  values <- series(function(fit) fit$GIC_values)
  sizes  <- series(function(fit) lengths(fit$selected_coeffs))
  cols <- rep(palette, times = length(fits))
  ltys <- rep(size_lty[names(fits)], each = length(palette))

  matplot(steps, values, type = "n", xlab = "Search step", ylab = "",
          main = "Criterion along the search")
  abline(v = passes, col = "grey85", lwd = 0.8)
  matlines(steps, values, type = "s", lwd = 2, col = cols, lty = ltys)
  title(ylab = "Criterion value", line = 3.4)

  matplot(steps, sizes, type = "n", ylim = range(c(sizes, true_size)),
          xlab = "Search step", ylab = "",
          main = sprintf("Model size (true size = %d)", true_size))
  abline(v = passes, col = "grey85", lwd = 0.8)
  abline(h = true_size, lty = 2, col = "grey35")
  matlines(steps, sizes, type = "s", lwd = 2, col = cols, lty = ltys)
  title(ylab = "Number of predictors", line = 3.4)

  # Legend rows under both panels: criterion colours, then sample-size line types
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot.new()
  legend("bottom", inset = c(0, 0.075), legend = names(palette), col = palette,
         lty = 1, lwd = 2.5, horiz = TRUE, bty = "n", cex = 0.95, seg.len = 2.5)
  legend("bottom", inset = c(0, 0.005), legend = c("n = 1000", "n = 2000"),
         col = "grey25", lty = size_lty, lwd = 2, horiz = TRUE, bty = "n",
         cex = 0.95, seg.len = 3)
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
            truth <- if (!is.null(env$true_union) && identical(m, "README-multivariate.png"))
              env$true_union else env$true_cols
            plot_criteria(env$fits, env$p, length(truth), file.path(figure_dir, m))
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

process("README.md", "^## Examples", "man/figures", draw = TRUE)
process("vignettes/examples.Rmd", "^## Helpers", "man/figures", draw = FALSE)
