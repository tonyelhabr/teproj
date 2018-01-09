
#' @title Render project output.
#' @description Renders R scripts with Roxygen comments into specified output format.
#' @details This is a wrapper for `rmarkdown::render()` (and `knitr::spin()`, if `keep_rmd == TRUE`).
#' It is designed specifically to convert R scripts formatted with Roxgyen comments
#' into the specified output format. (In other words, it mirrors the behavior of `knitr::knit()`
#' for a R markdown document.
#' @param filepaths_input character. Can be a vector.
#' @param dir_input character. Should not be a vector. Only used if `filepaths_input` is missing.
#' @param ... dots. Parameters passed to `list.files()`.
#' @param dir_output character. Should not be a vector. IMPORTANT: Relative to input directory (so something like "../output" is valid).
#' @param filenames_output character. Explicit filenames to use for output. Length of variable must exactly match number of filepaths meeting `list.files()` criteria.
#' @param rgx_input character. Alias to `pattern` parameter for `list.files()`. Used ONLY if `filepaths` is missing and `dir` is not.
#' @param rgx_input_include character. Regular expression to use to filter for `list.files()` output. (Somewhat redundant.)
#' @param rgx_input_exclude character. Regular expression to use to filter for `list.files()` output. (Somewhat redundant.)
#' @param rgx_output_trim character. Used ONLY if `filenames_output` is missing. Describes how input file names should be "trimmed" to make the output file name appear "cleaner".
#' @param ext_output character. File extension of output.
#' @param render boolean. Indiciates whether or not to actually carry out function.
#' @param return boolean. Relevant ONLY if `render == FALSE`. Set to `TRUE` in order to preview what would be rendered.
#' @param overwrite boolean. Indicates whether or not to overwrite any existing file with the same name. Not currently used.
#' @param quiet boolean. Direct argument for `rmarkdown::render()`.
#' @param backup boolean. Indicates whether or not to create e a backup.
#' @param backup_suffix character. Suffix to append to filename for backup file.
#' @param keep_rmd boolean. Assuming specified output is not ".Rmd", indicates whether to call `knitr::spin` to keep "intermediate" results.
#' @return data.frame. Information regarding what was rendered.
#' @export
#' @importFrom tibble tibble
#' @importFrom knitr spin
#' @importFrom rmarkdown render
render_proj_io <-
  function(filepaths_input,
           dir_input,
           ...,
           dir_output = NULL,
           filenames_output,
           rgx_input = ".",
           rgx_input_include = rgx_input,
           rgx_input_exclude = "",
           rgx_output_trim = "-v[0-9]+.R",
           ext_output = ".html",
           render = TRUE,
           return = TRUE,
           overwrite = TRUE,
           quiet = FALSE,
           backup = FALSE,
           backup_suffix = paste0("_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S")),
           keep_rmd = FALSE) {
    # # Debugging...
    # filepaths_input = NULL
    # dir_input = "tests/testthat/project-render"
    # rgx_input = ".R$"
    # rgx_input_include = rgx_input
    # rgx_input_exclude = "fake news"
    # rgx_output_trim = "-v[0-9]+.R"
    # dir_output = "tests/testthat/project-render/output/"
    # ext_output = ".html"
    # render = TRUE
    # rgx_render = "."
    # overwrite = TRUE
    # backup = FALSE
    # backup_suffix = paste0("_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
    # keep_md = FALSE

    # Check for render at the very end.
    if (!render && !return) {
      .print_argfalse_msg("render")
      return(invisible())
    }

    if(is.null(dir_output)) {
      .print_isnull_msg("dir_output")
      return(invisible())
    }

    files_exist <- .check_files_exist(filepaths = filepaths_input, dir = dir_input, pattern = rgx_input, ...)

    # browser()
    if(!files_exist$exist) {
      return(invisible())
    } else {
      filepaths_input <- files_exist$filepaths
    }

    # Secondary filtering...
    # browser()
    if (!missing(rgx_input_include))
      filepaths_input <-
      grep(rgx_input_include,
           filepaths_input,
           value = TRUE)
    filepaths_input

    if (!missing(rgx_input_exclude))
      filepaths_input <-
      grep(rgx_input_exclude,
           filepaths_input,
           value = TRUE,
           invert = TRUE)
    filepaths_input

    filepaths_exist <- as.logical(lapply(filepaths_input, file.exists))
    if(!any(filepaths_exist)) {
      if(getOption("teutils.print_wrn")) {
        .print_nofile_msg()
      }
      return(invisible())
    }

    # filenames_output <- gsub("^.*\\\\|^.*\\/", "", filenames_output)
    filenames_output <- basename(filepaths_input)
    filenames_output <- tools::file_path_sans_ext(filenames_output)
    filenames_output

    # browser()
    if (!missing(rgx_output_trim))
      filenames_output <-
      gsub(rgx_output_trim, "", filenames_output)
    filenames_output

    # filepaths_output <- paste0(dir_output, filename_output, ext_output)
    filepaths_output <-
      file.path(dir_output, paste0(filenames_output, ext_output))
    filepaths_output

    filepaths_output_backup <-
      gsub(ext_output,
           paste0(backup_suffix, ext_output),
           filepaths_output)
    filepaths_output_backup

    input <- output <- output_backup <- NULL
    filepaths_render_info <-
      tibble::tibble(input = filepaths_input,
                     # input_name = filenames_input,
                     output = filepaths_output,
                     # output_name = filenames_output,
                     output_backup = filepaths_output_backup)
    if(!backup)
      filepaths_render_info <- filepaths_render_info[, -ncol(filepaths_render_info)]
    filepaths_render_info

    if (render) {

      create_dir(dir_output)
      # create_dir(dir_output, overwrite = overwrite)

      i <- 1
      while (i <= nrow(filepaths_render_info)) {
        try({
          filepath_i <- filepaths_render_info$input[i]
          filepath_output_i <- filepaths_render_info$output[i]

          if (keep_rmd == TRUE) {
            knitr::spin(filepath_i, knit = FALSE)
            file.copy(from = filepaths_render_info$input_rmd[i],
                      to = filepaths_render_info$output_rmd[i])
            unlink(filepaths_render_info$input_rmd[i])
          }

          rmarkdown::render(
            input = filepath_i,
            output_file = filepath_output_i,
            quiet = quiet,
            output_options = set_pkg_render_opts()
          )

          if (backup) {
            if (file.exists(filepath_output_i)) {
              filepath_output_i_backup <- filepaths_render_info$output_backup[i]
              file.copy(from = filepath_output_i, to = filepath_output_i_backup)
            }
          }
        })
        i <- i + 1
      }
    }
    invisible(filepaths_render_info)
  }
