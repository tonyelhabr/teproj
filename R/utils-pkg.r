

# NOTE:
# Perhaps keep an option in .onAttach in a zzz.r file?)

.print_isnull_msg <- function(pkg_print_opts) {
  if(pkg_print_opts$wrn) warning("Missing required input. Returning nothing")
}

.print_argfalse_msg <- function(arg, pkg_print_opts) {
  if(pkg_print_opts$msg) message("Returning nothing because `", arg, " = FALSE`.")
}

# .print_dpc_msg <- function(pkg_print_opts) {
#   if(pkg_print_opts$msg) message("This function is deprecated.")
# }

.print_dpc_msg <- function(f, pkg_print_opts) {
  if(missing(f)) f <- as.character(NULL)
  if(pkg_print_opts$msg) message("This function is deprecated. Use ", f, "instead.")
}

.print_export_msg <- function(filepath, pkg_print_opts) {
  if(pkg_print_opts$msg) message("Saved ", basename(filepath), " as ", filepath, ".")
}

.print_usedefault_msg <-
  function(arg, arg_name = deparse(substitute(arg)), pkg_print_opts) {
    if(pkg_print_opts$msg) message("Using ", arg, " for ", arg_name, ".")
  }
