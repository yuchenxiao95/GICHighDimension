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

palette  <- c(AIC = "#2a78d6", BIC = "#eb6834", GIC5 = "#1baf7a", GIC6 = "#eda100")
linetype <- c(AIC = 1, BIC = 2, GIC5 = 4, GIC6 = 5)

# Two panels: each criterion's value, and its model size, after every search
# step. Dotted lines mark a new pass over the predictors; the dashed line marks
# the true model size.
plot_criteria <- function(fits, p, true_size, file) {
  png(file, width = 8, height = 3.7, units = "in", res = 200)
  on.exit(dev.off())
  op <- par(mfrow = c(1, 2), mar = c(4.2, 4.6, 2.2, 1), oma = c(1.6, 0, 0, 0), las = 1,
            cex.axis = 0.95, cex.lab = 1.1, cex.main = 1.1)
  on.exit(par(op), add = TRUE)

  steps  <- seq_along(fits[[1]]$GIC_values)
  passes <- seq(p, length(steps) - 1, by = p)
  values <- sapply(fits, `[[`, "GIC_values")
  sizes  <- sapply(fits, function(f) lengths(f$selected_coeffs))

  matplot(steps, values, type = "s", lwd = 2, col = palette, lty = linetype,
          xlab = "Search step", ylab = "", main = "Criterion along the search")
  title(ylab = "Criterion value", line = 3.4)
  abline(v = passes, lty = 3, col = "grey60")

  log_y <- max(sizes) > 50       # keep the end of the path visible
  matplot(steps, sizes, type = "s", lwd = 2, col = palette, lty = linetype,
          ylim = range(c(sizes, true_size)), log = if (log_y) "y" else "",
          xlab = "Search step", ylab = "",
          main = sprintf("Model size (true size = %d)", true_size))
  title(ylab = if (log_y) "Variables in model (log scale)" else "Variables in model",
        line = 3.4)
  abline(v = passes, lty = 3, col = "grey60")
  abline(h = true_size, lty = 2, col = "grey35")

  # One legend row under both panels, so it never covers a curve
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot.new()
  legend("bottom", legend = names(fits), col = palette, lty = linetype, lwd = 2,
         horiz = TRUE, bty = "n", cex = 0.95, seg.len = 3, inset = 0.01)
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
