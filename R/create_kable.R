
#' @source \url{https://stackoverflow.com/questions/29465941/format-number-in-r-with-both-comma-thousands-separator-and-specified-decimals}.
format_total <- function(x = NULL, digits = 0, nsmall = 0, big.mark = ",") {
  format(round(as.numeric(x), digits), nsmall = nsmall, big.mark = big.mark)
}

#' Create a \code{knitr::kable} object
#'
#' @description Creates a \code{knitr::kable} object.
#' @details None.
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

    stopifnot(is.data.frame(data))

    # NOTE: This ensures that `n_footnote` is evaluated priort to the filter of the input data.
    ret <- data
    if (show_footnote & (n_show < nrow(data))) {
      ret <- ret[1:n_show,]
    }

    ret <- knitr::kable(ret, format = format, escape = FALSE)

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

# TODO: Make sure rows are unique!
# #' Create a \code{knitr::kable} object
# #'
# #' @description Creates a \code{knitr::kable} kable object.
# #' This function is designed to allow the user to add specific rows
# #' to the output from \code{create_kable()} by specifying a column and a regular expression
# #' with which to filter the column.
# #' @details Defaults which are probably desired by the user are specified.
# #' @inheritParams create_kable
# #' @param col character. Name of column in \code{data} to which to apply string detection.
# #' @param rgx character. Regular expression used to filter \code{data} (based on values in \code{col_rgx}
# #' @export
# create_kable_filt_at <-
#   function(data = NULL,
#            col = NULL,
#            rgx = NULL,
#            n_show = 20L,
#            show_footnote = TRUE,
#            n_footnote = nrow(data),
#            ...) {
#
#     stopifnot(is.data.frame(data))
#     stopifnot(is.character(col), col %in% names(data))
#     stopifnot(is.character(rgx))
#     # data <- filter(data, grepl(rgx, !!rlang::sym(col)))
#
#     data_filt <- data
#     if(nrow(data_filt) > n_show) {
#       data_filt <- data_filt[1:n_show, ]
#     }
#
#     col_select <- data[,col]
#     idx_select <- grepl(rgx, col_select)
#     data_match <- data[idx_select, ]
#
#     nrow_match <- nrow(data_match)
#     n_kable <- n_show + nrow_match
#     data_kable <- rbind(data_filt, data_match)
#
#     ret <-
#       create_kable(data_kable,
#                    n_show = n_kable,
#                    show_footnote = show_footnote,
#                    n_footnote = n_footnote,
#                    ...)
#     ret
#   }

