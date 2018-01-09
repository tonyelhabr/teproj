

# References:
# 1. https://github.com/hrbrmstr/hrbrthemes/blob/master/R/zzz.r.
# 2. https://github.com/metrumresearchgroup/sinew/blob/master/R/zzz.R
# 3. https://github.com/hadley/devtools/blob/master/R/zzz.r.

.print_pkgstartupmsg <- function(msg) {
  paste0(strwrap(msg, width = 80), sep = "", collapse = "\n")
}

teutils_default_opts <- list(
  teutils.print_msg = TRUE,
  teutils.print_wrn = TRUE,
  teutils.print_err = TRUE
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(teutils_default_opts) %in% names(op))
  if(any(toset)) options(teutils_default_opts[toset])

  # invisible()
}
