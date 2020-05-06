create_plots <- function(..., add_title = TRUE) {
  plot_code <- paste(deparse(substitute(...)), collapse = " ")
  steps <- trimws(strsplit(plot_code, "\\+")[[1L]])

  n_steps <- length(steps)
  plots <- vector("list", n_steps)
  for (i in 1:n_steps) {
    code <- parse(text = steps[i])
    if (i == 1L) {
      plots[[i]] <- eval(code)
    } else {
      plots[[i]] <- plots[[i-1]] + eval(code)
    }
    if (add_title) {
      title <- if (i == 1L) steps[i] else paste("+", steps[i])
      plots[[i]] <- plots[[i]] +
        ggtitle(title) +
        theme(plot.title = element_text(family = "mono"))
    }
  }
  plots
}

teach_ggplot <- function(..., add_title = TRUE) {
  plots <- create_plots(..., add_title = add_title)
  patchwork::wrap_plots(plots) +
    patchwork::plot_annotation(
      tag_levels = "1",
      tag_prefix = "(",
      tag_suffix = ")"
    )
}

teach_ggplot_gif <- function(...,
                             add_title = TRUE,
                             output_file = "animation.gif",
                             delay = 3) {
  plots <- create_plots(..., add_title = add_title)
  gifski::save_gif(
    lapply(plots, print),
    gif_file = output_file,
    delay = delay
  )
}
