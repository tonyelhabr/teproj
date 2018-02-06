

#' Render project output
#'
#' @description Renders R scripts with Roxygen comments into specified output format.
#' @details This is a wrapper for \code{rmarkdown::render()}` (and \code{knitr::spin()}, if \code{keep_rmd == TRUE}).
#' It is designed specifically to convert R scripts formatted with Roxgyen comments
#' into the specified output format. (In other words, it mirrors the behavior of \code{knitr::knit()}
#' for a R markdown document.
#' @param filepath_input character. Can be a vector.
#' @param dir_input character. Should not be a vector. Only used if \code{filepath_input} is missing.
#' @param ... dots. Parameters passed to \code{list.files()}.
#' @param dir_output character. Should not be a vector. IMPORTANT: Relative to input directory (so something like "../output" is valid).
#' @param filename_output character. Explicit filenames to use for output. Length of variable must exactly match number of filepath meeting \code{list.files()} criteria.
#' @param rgx_input character. Alias to \code{pattern} parameter for \code{list.files()}. Used ONLY if \code{filepath} is missing and \code{dir} is not.
#' @param rgx_input_include character. Regular expression to use to filter for \code{list.files()} output. (Somewhat redundant.)
#' @param rgx_input_exclude character. Regular expression to use to filter for \code{list.files()} output. (Somewhat redundant.)
#' @param rgx_output_trim character. Used ONLY if \code{filename_output} is missing. Describes how input file names should be "trimmed" to make the output file name appear "cleaner".
#' @param ext_output character. File extension of output.
#' @param render boolean. Indiciates whether or not to actually carry out function.
#' @param render_params list. Parameters to pass directly to \code{params} argument of \code{rmarkdown::render()}
#' @param return boolean. Relevant ONLY if \code{render == FALSE}.
#' Set to \code{TRUE} in order to preview what would be rendered.
#' @param overwrite boolean. Indicates whether or not to overwrite any existing file with the same name. Not currently used.
#' @param quiet boolean. Direct argument for \code{rmarkdown::render()}.
#' @param backup boolean. Indicates whether or not to create e a backup.
#' @param backup_suffix character. Suffix to append to filename for backup file.
#' @param keep_rmd boolean. Assuming specified output is not ".Rmd", indicates whether to call \code{knitr::spin} to keep "intermediate" results.
#' @return data.frame. Information regarding what was rendered.
#' @export
#' @importFrom tibble tibble
#' @importFrom knitr spin
#' @importFrom rmarkdown render
#' @importFrom knitr opts_chunk
#' @importFrom tools file_path_sans_ext
render_proj_io <-
  function(filepath_input,
           dir_input,
           ...,
           dir_output = file.path(getwd()),
           filename_output,
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
      .print_argfalse_msg("render")
      return(invisible())
    }

    if(missing(filepath_input) & missing(dir_input)) {
      .print_ismiss_msg()

    } else if (missing(filepath_input) & !missing(dir_input)) {
      # TODO: For some reason include.dirs = FALSE does not exclude directories?
      filepath_input <-
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
      filepath_input <- setdiff(filepath_input, subdirs_input)
      if(length(filepath_input) == 0) {
        .print_nofile_msg()
        return(invisible())
      }
    } else {
      filepath_input_exist <- as.logical(lapply(filepath_input, file.exists))
      if(!any(filepath_input_exist)) {
        if(getOption("teutils.print.wrn")) {
          warning("Specified file(s) do not exist.")
          return(invisible())
        }
      }
    }

    # browser()
    # filepath_input <- normalizePath(filepath_input)
    filepath_input <- .normalize_path(filepath_input, mustWork = FALSE)

    # Secondary filtering...
    if (!missing(rgx_input_include)) {
      filepath_input <-
        grep(rgx_input_include,
             filepath_input,
             value = TRUE)
    }
    filepath_input

    if (!missing(rgx_input_exclude)) {
      filepath_input <-
        grep(rgx_input_exclude,
             filepath_input,
             value = TRUE,
             invert = TRUE)
    }

    filepath_exist <-
      as.logical(lapply(filepath_input, file.exists))
    if (!any(filepath_exist)) {
      .print_nofile_msg()
      return(invisible())
    }


    if(missing(filename_output)) {
      filename_output <- basename(filepath_input)
      filename_output <- tools::file_path_sans_ext(filename_output)
    }

    if (!missing(rgx_output_trim)) {
      filename_output <- gsub(rgx_output_trim, "", filename_output)
    }

    ext_output <- match.arg(ext_output)
    filepath_output <-
      .get_filepath(dir_output, filename_output, ext_output, filepath = NULL)
    filepath_output <- .normalize_path(filepath_output, mustWork = FALSE)

    filepath_output_backup <-
      gsub(ext_output,
           paste0(backup_suffix, ext_output),
           filepath_output)

    input <- output <- output_backup <- NULL
    filepath_render_info <-
      tibble::tibble(input = filepath_input,
                     # input_name = filename_input,
                     output = filepath_output,
                     # output_name = filename_output,
                     output_backup = filepath_output_backup)
    if (!backup) {
      filepath_render_info <- filepath_render_info[, -ncol(filepath_render_info)]
    }

    if (render) {
      # create_dir(dir_output, overwrite = overwrite)
      create_dir(dir_output, overwrite = FALSE)

      i <- 1
      while (i <= nrow(filepath_render_info)) {
        try({
          filepath_i <- filepath_render_info$input[i]
          filepath_output_i <- filepath_render_info$output[i]

          if (keep_rmd == TRUE) {

           #  browser()
            filepath_rmd_init_i <- knitr::spin(filepath_i, knit = FALSE)
            filepath_rmd_output_i <- gsub(ext_output, "Rmd", filepath_output_i)
            file.copy(from = filepath_rmd_init_i,
                      to = filepath_rmd_output_i)
            unlink(filepath_rmd_init_i)
          }

          opts <-
            list(knitr::opts_chunk$set(get_pkg_opts_renamed(type = "render")))
          rmarkdown::render(
            input = filepath_i,
            output_file = filepath_output_i,
            params = render_params,
            quiet = quiet,
            output_options = opts
          )

          if (backup) {
            if (file.exists(filepath_output_i)) {
              filepath_output_i_backup <- filepath_render_info$output_backup[i]
              file.copy(from = filepath_output_i, to = filepath_output_i_backup)
            }
          }
        })
        i <- i + 1
      }
    }
    invisible(filepath_render_info)
  }
