
.print_isnull_msg <- function(pkg_print_opts) {
  if(getOption("teproj.print_wrn")) warning("Missing required input. Returning nothing")
}

.print_argfalse_msg <- function(arg) {
  if(getOption("teproj.print_msg")) message("Returning nothing because `", arg, " = FALSE`.")
}

.print_dpc_msg <- function(f) {
  if(missing(f)) f <- as.character(NULL)
  if(getOption("teproj.print_msg")) message("This function is deprecated. Use ", f, "instead.")
}

.print_usedefault_msg <-
  function(arg, arg_name = deparse(substitute(arg))) {
    if(getOption("teproj.print_msg")) message("Using ", arg, " for ", arg_name, ".")
  }
