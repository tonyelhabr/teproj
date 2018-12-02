
#' @source <https://stackoverflow.com/questions/29465941/format-number-in-r-with-both-comma-thousands-separator-and-specified-decimals>.
.format_total <- function(x = NULL, digits = 0, nsmall = 0, big.mark = ",") {
  format(round(as.numeric(x), digits), nsmall = nsmall, big.mark = big.mark)
}

#' Create a `knitr::kable` object
#'
#' @description Creates a `knitr::kable` object.
#' @details None.
#' @param data data.frame.
#' @param n_show integer. Number of rows to show in output.
#' @param show_footnote logical. Whether to show total number of rows if `data` is truncated.
#' @param n_footnote integer. Number of rows to show in footnote. Only relevant if `show_footnote = TRUE`.
#' This is mostly useful if `data` is truncated before being passed to this function,
#' yet the user still wants to show an untruncated number by setting this value explicitly.
#' @param format character. Passed directly to same `knitr::kable()` argument.
#' @param full_width logical. Passed directly to same `kableExtra::kable_styling()` argument.
#' @param position character. Passed directly to same `kableExtra::kable_styling()` argument.
#' @param ... dots. Not currently used.
#' @return kable object.
#' @export
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling add_footnote
create_kable <-
  function(data,
           n_show = 20L,
           show_footnote = ifelse(nrow(data) > n_show, TRUE, FALSE),
           n_footnote = nrow(data),
           format = "html",
           ...,
           full_width = FALSE,
           position = "center") {

    stopifnot(is.data.frame(data))

    # NOTE: This ensures that `n_footnote` is evaluated priort to the filter of the input data.
    res <- data
    if (show_footnote & (n_show < nrow(data))) {
      res <- res[1:n_show,]
    }

    res <- knitr::kable(res, format = format, escape = FALSE)

    if (format == "html") {
      res <-
        kableExtra::kable_styling(
          res,
          full_width = full_width,
          position = position
        )

      if (show_footnote) {
        res <-
          kableExtra::add_footnote(
            res,
            c(sprintf("# of total rows: %s", .format_total(n_footnote))),
            notation = "number"
          )
      }
    }
    res
  }

#' @rdname create_kable
#' @export
create_kable_html <-
  function(..., format = "html") {
    create_kable(..., format = format)
  }

#' @rdname create_kable
#' @export
create_kable_md <-
  function(..., format = "markdown") {
    create_kable(..., format = format)
  }
