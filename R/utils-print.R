
.get_msg_input <-
  function(..., msg_input = "") {
    dots <- list(...)
    if (length(dots) > 0) {
      msg_input <- gsub(",$", "", paste(dots, collapse = ","))
      msg_input <- sprintf("`%s` ", msg_input)
    }
    msg_input
  }

.print_isnull_msg <-
  function(...,  msg_input = "", n = 2) {
    # parent.call <- sys.call(sys.nframe() - 1L)
    msg_input <- .get_msg_input(..., msg_input = msg_input)
    .warningf("Required input `%s`is NULL.", msg_input, n = n)
  }

.print_ismiss_msg <-
  function(...,  msg_input = "", n = 2) {
    dots <- list(...)
    msg_input <- .get_msg_input(..., msg_input = msg_input)
    .warningf("Required input `%s`is missing.", msg_input, n = n)
  }


.print_nofile_msg <-
  function(...,  msg_input = "", n = 2) {
    dots <- list(...)
    msg_input <- .get_msg_input(..., msg_input = msg_input)
    .warningf("Could not find any files meeting criteria `%s`.", msg_input, n = n)
  }

.print_filenotexist_msg <-
  function(path, n = 2) {
    .warningf("Cannot find `%s`", path, n = n)
  }


.print_argfalse_msg <-
  function(arg = NULL) {
    parent.call <- sys.call(sys.nframe() - 1L)
    message(sprintf("Returning nothing because `%s = FALSE`.", arg))
  }

.print_dpc_msg <-
  function(f = NULL) {
    parent.call <- sys.call(sys.nframe() - 1L)
    if (missing(f)) {
      f <- as.character(NULL)
    }
    message(sprintf("This function is deprecated. Please use %s instead.", f))
  }

.print_usedefault_msg <-
  function(arg = NULL,
           arg_name = deparse(substitute(arg))) {
    parent.call <- sys.call(sys.nframe() - 1L)
    message(sprintf("Using %s for %s.", arg, arg_name))
  }

.print_ignore_msg <-
  function(..., msg_input = "") {
    parent.call <- sys.call(sys.nframe() - 1L)
    dots <- list(...)
    if (length(dots) > 0) {
      msg_input <- gsub(",$", " ", paste(names(dots), collapse = ","))
    }
    message(sprintf("Ingoring parameters: %s.", msg_input))
  }

.print_nonreadr_msg <-
  function(pkg = NULL,
           ...,
           msg_input = "",
           n = 2) {
    dots <- list(...)
    if (length(dots) > 0) {
      msg_input <- gsub(",$", " ", paste(dots, collapse = ","))
    }
    message(sprintf("Using {%s} function instead of `{readr}` function.", pkg))
  }
