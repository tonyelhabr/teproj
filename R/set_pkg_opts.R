
#' Control package options
#'
#' @description Sets options for relevant \code{teproj} functions.
#' @details Intended to be used as a wrapper to \code{options(...)}.
#' @param msg,wrn,err booleans. Indiciates whether to show messages, warnings, and errors for package functions.
#' @rdname set_pkg_opts
#' @export
set_pkg_print_opts <- function(msg = getOption("teproj.print.msg"),
                               wrn = getOption("teproj.print.wrn"),
                               err = getOption("teproj.print.err")) {
  options(teproj.print.msg = msg)
  options(teproj.print.wrn = wrn)
  options(teproj.print.err = err)
}

#' Control package settings
#'
#' @description Sets options for \code{teproj::render_proj_io}.
#' @details Intended to be used as a wrapper to \code{options(...)}.
#' @param echo,cache,results,width,fig.align,fig.show,fig.width,fig.height,out.width,out.height,warning,message Arguments passed to the \code{knitr_opts$set()} arguments of their same namesake.
#' @export
set_pkg_render_opts <-
  function(echo = getOption("teproj.render.echo"),
           cache = getOption("teproj.render.cache"),
           results = getOption("teproj.render.results"),
           width = getOption("teproj.render.width"),
           fig.align = getOption("teproj.render.fig.align"),
           fig.show = getOption("teproj.render.fig.show"),
           fig.width = getOption("teproj.render.fig.width"),
           fig.height = getOption("teproj.render.fig.height"),
           out.width = getOption("teproj.render.out.width"),
           out.height = getOption("teproj.render.out.height"),
           warning = getOption("teproj.render.warning"),
           message = getOption("teproj.render.message")) {
    options(teproj.render.echo = echo)
    options(teproj.render.cache = cache)
    options(teproj.render.results = results)
    options(teproj.render.width = width)
    options(teproj.render.fig.align = fig.align)
    options(teproj.render.fig.show = fig.show)
    options(teproj.render.fig.width = fig.width)
    options(teproj.render.fig.height = fig.height)
    options(teproj.render.out.width = out.width)
    options(teproj.render.out.height = out.height)
    options(teproj.render.warning = warning)
    options(teproj.render.message = message)
  }

#' Control package option
#' .
#' @description Sets options for saving a \code{ggplot2} plot with \code{teproj::export()}.
#' @details Intended to be used as a wrapper to \code{options(...)}.
#' @param units character. \code{ggsave()} parameters.
#' @param width,height numerics. \code{ggsave()} parameters.
#' @export
set_pkg_ggssave_opts <- function(units = getOption("teproj.ggsave.units"),
                                 width = getOption("teproj.ggsave.width"),
                                 height = getOption("teproj.ggsave.height")) {
  options(teproj.ggsave.units = units)
  options(teproj.ggsave.width = width)
  options(teproj.ggsave.height = height)
}
