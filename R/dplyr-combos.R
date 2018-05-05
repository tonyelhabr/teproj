
# arrange_distinct <-
#   function(data = NULL, ...) {
#
#     stopifnot(!is.null(data), is.data.frame(data))
#     # dots <- alist(...)
#     # stopifnot(length(dots) >= 1)
#
#     cols <- rlang::enquos(...)
#     stopifnot(rlang::is_quosures(cols), length(cols) >= 1)
#
#     data <- dplyr::distinct(data, !!!cols)
#     data <- dplyr::arrange(data, !!!cols)
#     data
#   }
#
# pull_distinct <-
#   function(data = NULL, col) {
#
#     stopifnot(!is.null(data), !s.data.frame(data))
#     stopifnot(rlang::is_quosure(col), length(col) == 1, any(names(data) == col))
#     col <- rlang::enquo(col)
#     data <- dplyr::distinct(data, !!col)
#     data <- dplyr::arrange(data, !!col)
#     data <- dplyr::pull(data, !!col)
#     data
#   }
