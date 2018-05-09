
#' @source \url{https://stackoverflow.com/questions/29465941/format-number-in-r-with-both-comma-thousands-separator-and-specified-decimals}.
format_total <- function(x = NULL, digits = 0, nsmall = 0, big.mark = ",") {
  format(round(as.numeric(x), digits), nsmall = nsmall, big.mark = big.mark)
}

#' Create a directory
#'
#' @description Creates the directory if it does not exist.
#' @details Used by other functions in this package.
#' Note that this function is probably more complex than it really should be.
#' Re-factoring it should be considered.
#' @param data data.frame.
#' @param n_show integer. Number of rows to show in output.
#' @param show_footnote logical. Whether to show total number of rows if \code{data} is truncated.
#' @param n_footnote integer. Number of rows to show in footnote. Only relevant if \code{show_footnote = TRUE}.
#' This is mostly useful if \code{data} is truncated before being passed to this function,
#' yet the user still wants to show an untruncated number by setting this value explicitly.
#' @param format character. Passed directly to same \code{knitr::kable()} argument.
#' @param full_width logical. Passed directly to same \code{kableExtra::kable_styling()} argument.
#' @param position character. Passed directly to same \code{kableExtra::kable_styling()} argument.
#' @param ... dots. Not currently used.
#' @return kable object.
#' @export
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling add_footnote
create_kable <-
  function(data = NULL,
           n_show = 20L,
           show_footnote = ifelse(nrow(data) > n_show, TRUE, FALSE),
           n_footnote = nrow(data),
           format = "html",
           ...,
           full_width = FALSE,
           position = "center") {

    # browser()
    stopifnot(!is.null(data), is.data.frame(data))

    n_footnote <- nrow(data)
    n_show <- ifelse(n_show > n_footnote, n_footnote, n_show)
    if (show_footnote) {
      data <- data[1:n_show,]
    }

    ret <- knitr::kable(data, format = format, escape = FALSE)

    if (format == "html") {
      ret <-
        kableExtra::kable_styling(ret, full_width = full_width, position = position)
    }

    if (show_footnote) {
      ret <-
        kableExtra::add_footnote(ret, c(sprintf("# of total rows: %s", format_total(n_footnote))), notation = "number")
    }
    invisible(ret)
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

