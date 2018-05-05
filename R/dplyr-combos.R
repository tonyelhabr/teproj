
#' \code{dplyr::arrange()} + \code{dplyr::distinct()}
#'
#' @description Shorthand for \code{dplyr} functions called consecutively.
#' @details None.
#' @param data data.frame
#' @param ... dots. Bare names of columns in \code{data} on which to perform operations.
#' @return data.frame
#' @export
#' @importFrom rlang enquos is_quosures
#' @importFrom dplyr distinct arrange
arrange_distinctly <-
  function(data = NULL, ...) {

    stopifnot(!is.null(data), is.data.frame(data))
    # dots <- alist(...)
    # stopifnot(length(dots) >= 1)

    cols <- rlang::enquos(...)
    # if (length(cols) == 0)  {
    #   cols <- tidyselect::everything(data)
    # }
    stopifnot(rlang::is_quosures(cols), length(cols) >= 1)

    ret <- data
    ret <- dplyr::distinct(ret, !!!cols)
    ret <- dplyr::arrange(ret, !!!cols)
    ret
  }


#' \code{dplyr::distinct()} + \code{dplyr::arrange()} + \code{dplyr::pull()}
#'
#' @description Shorthand for \code{dplyr} functions called consecutively.
#' @details None.
#' @inheritParams summarise_stats
#' @return vector.
#' @export
#' @importFrom rlang is_quosure enquo
#' @importFrom dplyr distinct arrange pull
pull_distinctly <-
  function(data = NULL, col) {

    stopifnot(!is.null(data), is.data.frame(data))

    # stopifnot(rlang::is_quosure(col), length(col) == 1, any(names(data) == col))

    col <- rlang::enquo(col)
    # stopifnot(rlang::is_quosure(col), length(col) == 1, any(names(data) == col))

    ret <- data
    ret <- dplyr::distinct(ret, !!col)
    ret <- dplyr::arrange(ret, !!col)
    ret <- dplyr::pull(ret, !!col)
    ret
  }

#' Summary statistics for a data.frame
#'
#' @description A customized summary of a data.frame.
#' @details The purpose of this function is equivalent to that of similar functions in
#' other packages, such as \code{skimr::skim()}.
#' This function outputs the following \code{n}, \code{mean}, \code{median}, \code{sd},
#' \code{min}, \code{max}, \code{zn1}, \code{zp1}, \code{q25}, \code{q75}, \code{q05}, \code{q95}.
#'
#' The \code{_at} versions of this function are SE evaluation. (i.e. They take characters
#' as column names.) The \code{_by} version(s) of this function allows
#' for groups to be specificed as an input, although this function will detect groups
#' and respect their integrity  (meaning that the \code{_by} version(s) are simply
#' provided as an alternative means).
#'
#' @param data data.frame
#' @param col charactor for SE version; symbol for NSE version. Name of column in \code{data} on which to perform operations.
#' @param ... dots. Arguments passed to stats functions used internally.
#' @param na.rm logical. Argument passed to stats function used internally.
#' @param tidy logical. Whether to put output in long (i.e. tidy) format.
#' @return data.frame.
#' @export
#' @rdname summarise_stats
#' @seealso \url{https://github.com/ropenscilabs/skimr/blob/master/R/skim.R}.
#' @importFrom dplyr groups group_by mutate summarise_at vars funs first ungroup
#' @importFrom rlang syms sym
#' @importFrom stats median sd quantile
#' @importFrom tidyr gather
#' @importFrom tibble as_tibble
summarise_stats_at <-
  function(data = NULL,
           col = NULL,
           ...,
           na.rm = TRUE,
           tidy = FALSE) {

    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(col), length(col) == 1, is.character(col))
    stopifnot(length(intersect(names(data), col)) == 1)
    is_grouped <- ifelse(is.null(dplyr::groups(data)), FALSE, TRUE)
    if (is_grouped) {
      # browser()
      cols_grp_chr <- as.character(dplyr::groups(data))
      cols_grp <- rlang::syms(cols_grp_chr)
      data <- dplyr::group_by(data, !!!cols_grp)
    } else {
      cols_grp <- NULL
    }
    col <- rlang::sym(col)
    n <- . <- NULL

    data <- dplyr::mutate(data, n = sum(!is.na(!!col)))

    ret <-
      dplyr::summarise_at(
        data,
        dplyr::vars(!!col),
        dplyr::funs(
          n = dplyr::first(n),
          mean = mean(., na.rm = na.rm, ...),
          median = stats::median(., ...),
          sd = stats::sd(., na.rm = na.rm, ...),
          min = min(., na.rm = na.rm, ...),
          max = max(., na.rm = na.rm, ...),
          zn1 = mean(., na.rm = na.rm, ...) - stats::sd(., ...),
          zp1 = mean(., na.rm = na.rm, ...) + stats::sd(., na.rm = na.rm, ...),
          q25 = stats::quantile(., 0.25, na.rm = na.rm, ...),
          q75 = stats::quantile(., 0.75, na.rm = na.rm, ...),
          q05 = stats::quantile(., 0.05, na.rm = na.rm, ...),
          q95 = stats::quantile(., 0.95, na.rm = na.rm, ...)
        )
      )

    ret <- dplyr::ungroup(ret)

    if (tidy) {
      stat <- NULL
      value <- NULL
      # ret <- tidyr::gather(ret, stat, value)
      if (!is.null(cols_grp)) {
        ret <-
          suppressWarnings(tidyr::gather(ret, stat, value, -c(cols_grp_chr)))
      } else {
        ret <- suppressWarnings(tidyr::gather(ret, stat, value))
      }
      ret <- tibble::as_tibble(ret)
    }
    ret

  }

#' @rdname summarise_stats
#' @export
#' @importFrom rlang quo_text enquo
summarise_stats <-
  function(data = NULL,
           col,
           ...,
           tidy = FALSE) {
    # stopifnot(!missing(col), !is.character(col))
    # browser()
    summarise_stats_at(data = data,
                       col = rlang::quo_text(rlang::enquo(col)),
                       ...,
                       tidy = tidy)
  }

#' @rdname summarise_stats
#' @param cols_grp charactor for SE version; symbol for NSE version.
#' @export
#' @importFrom rlang syms
#' @importFrom dplyr group_by ungroup arrange
summarise_stats_by_at <-
  function(data = NULL,
           col = NULL,
           cols_grp = NULL,
           ...) {
    stopifnot(is.character(col))
    stopifnot(is.character(cols_grp))
    stopifnot(length(intersect(names(data), cols_grp)) == length(cols_grp))
    cols_grp <- rlang::syms(cols_grp)
    ret <- data
    ret <- dplyr::group_by(ret, !!!cols_grp)
    ret <- summarise_stats_at(ret, col, ...)
    ret <- dplyr::ungroup(ret)
    ret
  }


