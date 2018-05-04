

#' Render project output
#'
#' @description Renders R scripts with Roxygen comments into specified output format.
#' @details This is a wrapper for \code{rmarkdown::render()}` (and \code{knitr::spin()}, if \code{keep_rmd == TRUE}).
#' It is designed specifically to convert R scripts formatted with Roxgyen comments
#' into the specified output format. (In other words, it mirrors the behavior of \code{knitr::knit()}
#' for a R markdown document.
#' @param path_input character. Can be a vector.
#' @param dir_input character. Should not be a vector. Only used if \code{path_input} is missing.
#' @param ... dots. Parameters passed to \code{list.files()}.
#' @param dir_output character. Should not be a vector. IMPORTANT: Relative to input directory (so something like "../output" is valid).
#' @param basename_output character. Explicit basenames to use for output. Length of variable must exactly match number of path meeting \code{list.files()} criteria.
#' @param rgx_input character. Alias to \code{pattern} parameter for \code{list.files()}. Used ONLY if \code{path} is missing and \code{dir} is not.
#' @param rgx_input_include character. Regular expression to use to filter for \code{list.files()} output. (Somewhat redundant.)
#' @param rgx_input_exclude character. Regular expression to use to filter for \code{list.files()} output. (Somewhat redundant.)
#' @param rgx_output_trim character. Used ONLY if \code{basename_output} is missing. Describes how input file names should be "trimmed" to make the output file name appear "cleaner".
#' @param ext_output character. File extension of output.
#' @param render logical. Indiciates whether or not to actually carry ret function.
#' @param render_params list. Parameters to pass directly to \code{params} argument of \code{rmarkdown::render()}
#' @param return logical. Relevant ONLY if \code{render == FALSE}.
#' Set to \code{TRUE} in order to preview what would be rendered.
#' @param overwrite logical. Indicates whether or not to overwrite any existing file with the same name. Not currently used.
#' @param quiet logical. Direct argument for \code{rmarkdown::render()}.
#' @param backup logical. Indicates whether or not to create e a backup.
#' @param backup_suffix character. Suffix to append to file for backup file.
#' @param keep_rmd logical. Assuming specified output is not ".Rmd", indicates whether to call \code{knitr::spin} to keep "intermediate" results.
#' @return data.frame. Information regarding what was rendered.
#' @export
#' @importFrom tibble tibble
#' @importFrom knitr spin
#' @importFrom rmarkdown render
#' @importFrom knitr opts_chunk
#' @importFrom tools file_path_sans_ext
render_proj_io <-
  function(path_input,
           dir_input,
           ...,
           dir_output = file.path(getwd()),
           basename_output,
           rgx_input = ".",
           rgx_input_include = rgx_input,
           rgx_input_exclude = "",
           rgx_output_trim = "-v[0-9]+.R",
           ext_output = c("html"),
           render = TRUE,
           render_params = NULL,
           return = TRUE,
           overwrite = TRUE,
           quiet = FALSE,
           backup = FALSE,
           backup_suffix = paste0("_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S")),
           keep_rmd = FALSE) {

    # Check for render at the very end.
    if (!render & !return) {
      print_argfalse_msg("render")
      return(invisible())
    }

    if(missing(path_input) & missing(dir_input)) {
      print_ismiss_msg()

    } else if (missing(path_input) & !missing(dir_input)) {
      # TODO: For some reason include.dirs = FALSE does not exclude directories?
      path_input <-
        list.files(
          path = dir_input,
          pattern = rgx_input,
          full.names = TRUE,
          ignore.case = TRUE,
          include.dirs = FALSE
        )
      subdirs_input <-
        list.dirs(path = dir_input,
                  # ...,
                  recursive = FALSE,
                  full.names = TRUE
        )
      path_input <- setdiff(path_input, subdirs_input)
      if(length(path_input) == 0) {
        print_nofile_msg()
        return(invisible())
      }
    } else {
      path_input_exist <- as.logical(lapply(path_input, file.exists))
      if(!any(path_input_exist)) {
        if(getOption("teutils.print.wrn")) {
          warning("Specified file(s) do not exist.")
          return(invisible())
        }
      }
    }

    # browser()
    # path_input <- normalizePath(path_input)
    path_input <- normalize_path(path_input, mustWork = FALSE)

    # Secondary filtering...
    if (!missing(rgx_input_include)) {
      path_input <-
        grep(rgx_input_include,
             path_input,
             value = TRUE)
    }
    path_input

    if (!missing(rgx_input_exclude)) {
      path_input <-
        grep(rgx_input_exclude,
             path_input,
             value = TRUE,
             invert = TRUE)
    }

    path_exist <-
      as.logical(lapply(path_input, file.exists))
    if (!any(path_exist)) {
      print_nofile_msg()
      return(invisible())
    }


    if(missing(basename_output)) {
      basename_output <- file(path_input)
      basename_output <- tools::file_path_sans_ext(basename_output)
    }

    if (!missing(rgx_output_trim)) {
      basename_output <- gsub(rgx_output_trim, "", basename_output)
    }

    ext_output <- match.arg(ext_output)
    path_output <-
      get_path_safely(dir_output, basename_output, ext_output, path = NULL)
    path_output <- normalize_path(path_output, mustWork = FALSE)

    path_output_backup <-
      gsub(ext_output,
           paste0(backup_suffix, ext_output),
           path_output)

    input <- output <- output_backup <- NULL
    path_render_info <-
      tibble::tibble(input = path_input,
                     # input_name = basename_input,
                     output = path_output,
                     # output_name = basename_output,
                     output_backup = path_output_backup)
    if (!backup) {
      path_render_info <- path_render_info[, -ncol(path_render_info)]
    }

    if (render) {
      # create_dir(dir_output, overwrite = overwrite)
      create_dir(dir_output, overwrite = FALSE)

      i <- 1
      while (i <= nrow(path_render_info)) {
        try({
          path_i <- path_render_info$input[i]
          path_output_i <- path_render_info$output[i]

          if (keep_rmd == TRUE) {

           #  browser()
            path_rmd_init_i <- knitr::spin(path_i, knit = FALSE)
            path_rmd_output_i <- gsub(ext_output, "Rmd", path_output_i)
            file.copy(from = path_rmd_init_i,
                      to = path_rmd_output_i)
            unlink(path_rmd_init_i)
          }

          opts <-
            list(knitr::opts_chunk$set(get_pkg_opts_renamed(type = "render")))
          rmarkdown::render(
            input = path_i,
            output_file = path_output_i,
            params = render_params,
            quiet = quiet,
            output_options = opts
          )

          if (backup) {
            if (file.exists(path_output_i)) {
              path_output_i_backup <- path_render_info$output_backup[i]
              file.copy(from = path_output_i, to = path_output_i_backup)
            }
          }
        })
        i <- i + 1
      }
    }
    invisible(path_render_info)
  }
