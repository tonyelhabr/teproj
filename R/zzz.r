

# References:
# 1. https://github.com/hrbrmstr/hrbrthemes/blob/master/R/zzz.r.
# 2. https://github.com/metrumresearchgroup/sinew/blob/master/R/zzz.R
# 3. https://github.com/hadley/devtools/blob/master/R/zzz.r.

.print_pkgstartupmsg <- function(msg) {
  paste0(strwrap(msg, width = 80), sep = "", collapse = "\n")
}

pkg_knit_opts <- list(teproj.fig_width = 10)

pkg_print_opts <- list(
  teproj.print_msg = TRUE,
  teproj.print_wrn = TRUE,
  teproj.print_err = TRUE
)

pkg_default_opts <- c(pkg_print_opts, pkg_knit_opts)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(pkg_default_opts) %in% names(op))
  if(any(toset)) options(pkg_default_opts[toset])
  invisible()
}
