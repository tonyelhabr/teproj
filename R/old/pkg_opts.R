

#' #' View package options
#' #'
#' #' @description View options for relevant `teproj` functions.
#' #' @details Particularly useful for the `render_pojr_io()` function.
#' #' @param type character. Specification of which package options to extract (i.e. "print", "render", or "ggsave").
#' #' @param pkg_opts_prefix character. "teproj".
#' #' @return list (of named characters). Renamed package options.
#' get_pkg_opts_renamed <- function(type = c("print", "render", "ggsave"), pkg_opts_prefix = "teproj") {
#'   type <- match.arg(type)
#'   opts <- get_pkg_opts_verbose(type, pkg_opts_prefix)
#'   rgx <- paste0(pkg_opts_prefix, "\\.", type, "\\.")
#'   res <- opts
#'   names(res) <- gsub(rgx, "", names(opts))
#'   res
#' }
#'
#' get_pkg_opts_verbose <- function(type = c("print", "render", "ggsave"), pkg_opts_prefix = "teproj") {
#'   type <- match.arg(type)
#'   rgx <- paste0(pkg_opts_prefix, ".*", type)
#'   opts <- options()[grep(rgx, names(options()))]
#'   opts
#' }
#'
#' #' Control package options
#' #'
#' #' @description Sets options for relevant `teproj` functions.
#' #' @details Intended to be used as a wrapper to `options(...)`.
#' #' @param msg,wrn,err booleans. Indiciates whether to show messages, warnings, and errors for package functions.
#' set_pkg_print_opts <- function(msg = getOption("teproj.print.msg"),
#'                                wrn = getOption("teproj.print.wrn"),
#'                                err = getOption("teproj.print.err")) {
#'   options(teproj.print.msg = msg)
#'   options(teproj.print.wrn = wrn)
#'   options(teproj.print.err = err)
#' }
#'
#' #' Control package options
#' #'
#' #' @description Sets options for `teproj::render_proj_io`.
#' #' @details Intended to be used as a wrapper to `options(...)`.
#' #' @param echo,cache,results,fig.align,fig.show,fig.width,fig.height,warning,message
#' #' Arguments passed to the `knitr_opts$set()` arguments of their same namesake.
#' #' @export
#' set_pkg_render_opts <-
#'   function(echo = getOption("teproj.render.echo"),
#'            cache = getOption("teproj.render.cache"),
#'            results = getOption("teproj.render.results"),
#'            fig.align = getOption("teproj.render.fig.align"),
#'            fig.show = getOption("teproj.render.fig.show"),
#'            fig.width = getOption("teproj.render.fig.width"),
#'            fig.height = getOption("teproj.render.fig.height"),
#'            # res.width = getOption("teproj.render.res.width"),
#'            # res.height = getOption("teproj.render.res.height"),
#'            warning = getOption("teproj.render.warning"),
#'            message = getOption("teproj.render.message")) {
#'     options(teproj.render.echo = echo)
#'     options(teproj.render.cache = cache)
#'     options(teproj.render.results = results)
#'     options(teproj.render.fig.align = fig.align)
#'     options(teproj.render.fig.show = fig.show)
#'     options(teproj.render.fig.width = fig.width)
#'     options(teproj.render.fig.height = fig.height)
#'     # options(teproj.render.res.width = res.width)
#'     # options(teproj.render.res.height = res.height)
#'     options(teproj.render.warning = warning)
#'     options(teproj.render.message = message)
#'   }
#'
#' #' Control package options
#' #'
#' #' @description Sets options for saving a `ggplot2` plot with `teproj::export()`.
#' #' @details Intended to be used as a wrapper to `options(...)`.
#' #' @param units character. `ggsave()` parameters.
#' #' @param width,height numerics. `ggsave()` parameters.
#' set_pkg_ggssave_opts <- function(units = getOption("teproj.ggsave.units"),
#'                                  width = getOption("teproj.ggsave.width"),
#'                                  height = getOption("teproj.ggsave.height")) {
#'   options(teproj.ggsave.units = units)
#'   options(teproj.ggsave.width = width)
#'   options(teproj.ggsave.height = height)
#' }
