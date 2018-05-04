
#' Create a directory
#'
#' @description Creates the directory if it does not exist.
#' @details Used by other functions in this package.
#' Note that this function is probably more complex than it really should be.
#' Re-factoring it should be considered.
#' @param data data.frame.
#' @param n_show integer. Number of rows to show in output.
#' @param format character. Passed directly to same \code{knitr::kable()} argument.
#' @param full_width logical. Passed directly to same \code{kableExtra::kable_styling()} argument.
#' @param posiition character. Passed directly to same \code{kableExtra::kable_styling()} argument.
#' @param ... dots. Not currently used.
#' @return kable object.
#' @export
#' @importFrom knitr kable
#' @importFRom kableExtra kable_styling add_footnote
create_kable <-
  function(data = NULL,
           n_show = 20L,
           format = "html",
           show_footnote = ifelse(nrow(data) > n_show, TRUE, FALSE),
           ...,
           full_width = FALSE,
           position = "center") {

    # browser()
    stopifnot(!is.null(data), is.data.frame(data))

    n_rows <- nrow(data)
    n_show <- ifelse(n_show > n_rows, n_rows, n_show)
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
        kableExtra::add_footnote(ret, c(sprintf("# of total rows: %.0f", n_rows)), notation = "number")
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

