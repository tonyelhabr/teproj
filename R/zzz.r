
# References:
# 1. https://github.com/hrbrmstr/hrbrthemes/blob/master/R/zzz.r.
# 2. https://github.com/metrumresearchgroup/sinew/blob/master/R/zzz.R
# 3. https://github.com/hadley/devtools/blob/master/R/zzz.r.

.print_pkgstartupmsg <- function(msg) {
  paste0(strwrap(msg, width = 80), sep = "", collapse = "\n")
}

pkg_render_opts <- list(
  teproj.render_echo = FALSE,
  teproj.render_cache = FALSE,
  teproj.render_results = "hide",
  teproj.render_fig_align = "center",
  teproj.render_fig_show = "hide",
  teproj.render_width = 100,
  teproj.render_fig.width = 10,
  teproj.render_fig_height = 10,
  teproj.render_warning = FALSE,
  teproj.render_message = FALSE
)

pkg_ggsave_opts <- list(
  teproj.ggsave_units = "in",
  teproj.ggsave_width = 11,
  teproj.ggsave_height = 7
)

pkg_print_opts <- list(
  teproj.print_msg = TRUE,
  teproj.print_wrn = TRUE,
  teproj.print_err = TRUE
)

pkg_default_opts <-
  c(pkg_print_opts, pkg_ggsave_opts, pkg_render_opts)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(pkg_default_opts) %in% names(op))
  if (any(toset))
    options(pkg_default_opts[toset])
  invisible()
}
