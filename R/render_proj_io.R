
.get_render_opts <- function() {
  list(
    echo = FALSE,
    cache = FALSE,
    results = "hide",
    fig.align = "center",
    fig.show = "hide",
    # width = 100,
    fig.width = 10,
    fig.height = 10,
    warning = FALSE,
    message = FALSE
  )
}

#' @title Render project output.
#' @description Parses project to identify inputs/out variables, filenames, etc.
#' @details None.
#' @param filepaths_input character
#' @param dir_input character
#' @param ... character
#' @param dir_output character. File path.
#' @param filenames_output character.
#' @param rgx_input character.
#' @param rgx_input_include character.
#' @param rgx_input_exclude character.
#' @param rgx_output_trim character.
#' @param ext_output character.
#' @param render character.
#' @param rgx_render character
#' @param overwrite boolean.
#' @param backup character.
#' @param backup_suffix character.
#' @param keep_rmd character.
#' @return data.frame.
#' @export
#' @importFrom tibble tibble
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
           rgx_render = ".",
           overwrite = TRUE,
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
    # if (!render) {
    #   .print_argfalse_msg("render")
    #   return(invisible())
    # }

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

    if(!missing(rgx_render))
      filepaths_render_info <- subset(filepaths_render_info, grepl(rgx_render, output))

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
            # quiet = TRUE,
            output_options = .get_render_opts()
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
