
# References:
# 1. https://github.com/hrbrmstr/hrbrthemes/blob/master/R/zzz.r.
# 2. https://github.com/metrumresearchgroup/sinew/blob/master/R/zzz.R
# 3. https://github.com/hadley/devtools/blob/master/R/zzz.r.

# print_pkgstartupmsg <- function(msg) {
#   paste0(strwrap(msg, width = 80), sep = "", collapse = "\n")
# }

.PKG_RENDER_OPTS <- list(
  teproj.render.echo = FALSE,
  teproj.render.cache = FALSE,
  teproj.render.results = "hide",
  teproj.render.fig.align = "center",
  teproj.render.fig.show = "hide",
  teproj.render.fig.width = 10,
  teproj.render.fig.height = 10,
  # teproj.render.res.width = 10,
  # teproj.render.res.height = 10,
  teproj.render.warning = FALSE,
  teproj.render.message = FALSE
)

# pkg_ggsave_opts <- list(
#   teproj.ggsave.units = "in",
#   teproj.ggsave.width = 10,
#   teproj.ggsave.height = 10
# )
#
# pkg_print_opts <- list(
#   # teproj.print.msg = FALSE,
#   teproj.print.msg = TRUE,
#   teproj.print.wrn = TRUE,
#   teproj.print.err = TRUE
# )

# .PKG_DEFAULT_OPTS <-
#   c(pkg_print_opts, pkg_ggsave_opts, .PKG_RENDER_OPTS)
.PKG_DEFAULT_OPTS <- c(.PKG_RENDER_OPTS)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(.PKG_DEFAULT_OPTS) %in% names(op))
  if (any(toset))
    options(.PKG_DEFAULT_OPTS[toset])
  invisible()
}
