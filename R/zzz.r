
# References:
# 1. https://github.com/hrbrmstr/hrbrthemes/blob/master/R/zzz.r.
# 2. https://github.com/metrumresearchgroup/sinew/blob/master/R/zzz.R
# 3. https://github.com/hadley/devtools/blob/master/R/zzz.r.

.print_pkgstartupmsg <- function(msg) {
  paste0(strwrap(msg, width = 80), sep = "", collapse = "\n")
}

pkg_render_opts <- list(
  teproj.render.echo = FALSE,
  teproj.render.cache = FALSE,
  teproj.render.results = "hide",
  teproj.render.fig.align = "center",
  teproj.render.fig.show = "hide",
  teproj.render.fig.width = 10,
  teproj.render.fig.height = 10,
  # teproj.render.out.width = 10,
  # teproj.render.out.height = 10,
  teproj.render.warning = FALSE,
  teproj.render.message = FALSE
)

pkg_ggsave_opts <- list(
  teproj.ggsave.units = "in",
  teproj.ggsave.width = 10,
  teproj.ggsave.height = 10
)

pkg_print_opts <- list(
  # teproj.print.msg = FALSE,
  teproj.print.msg = TRUE,
  teproj.print.wrn = TRUE,
  teproj.print.err = TRUE
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
