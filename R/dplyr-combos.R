
arrange_distinct <-
  function(data = NULL, ...) {

    stopifnot(!is.null(data), is.data.frame(data))
    # dots <- alist(...)
    # stopifnot(length(dots) >= 1)

    cols <- rlang::enquos(...)
    stopifnot(rlang::is_quosures(cols), length(cols) >= 1)

    ret <- data
    ret <- dplyr::distinct(ret, !!!cols)
    ret <- dplyr::arrange(ret, !!!cols)
    ret
  }

pull_distinct <-
  function(data = NULL, col) {

    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(rlang::is_quosure(col), length(col) == 1, any(names(data) == col))
    col <- rlang::enquo(col)
    ret <- data
    ret <- dplyr::distinct(ret, !!col)
    ret <- dplyr::arrange(ret, !!col)
    ret <- dplyr::pull(ret, !!col)
    ret
  }

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
    }
    col <- rlang::sym(col)
    n <- NULL

    data <- mutate(data, n = sum(!is.na(!!col)))

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
          z_n1 = mean(., na.rm = na.rm, ...) - stats::sd(., ...),
          z_p1 = mean(., na.rm = na.rm, ...) + stats::sd(., na.rm = na.rm, ...),
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
      if (!missing(cols_grp)) {
        ret <-
          suppressWarnings(tidyr::gather(ret, stat, value, -c(cols_grp_chr)))
      } else {
        ret <- suppressWarnings(tidyr::gather(ret, stat, value))
      }
      ret <- tibble::as_tibble(ret)
    }
    ret

  }

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

summarise_stats_by_at <-
  function(data = NULL,
           col = NULL,
           cols_grp = NULL) {
    stopifnot(is.character(col))
    stopifnot(is.character(cols_grp))
    stopifnot(length(intersect(names(data), cols_grp)) == length(cols_grp))
    cols_grp <- rlang::syms(cols_grp)
    ret <- data
    ret <- dplyr::group_by(ret, !!!cols_grp)
    ret <- summarise_stats_at(ret, col)
    ret <- dplyr::ungroup(ret)
    ret <- dplyr::arrange(ret, !!!cols_grp)
    ret
  }


