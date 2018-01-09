
.remove_rgx <- function(char, rgx) {
  regmatches(char, regexpr(rgx, char))
}

.print_parse_proj_io_msg <-
  function(action,
           var,
           line_idx,
           line,
           filename,
           ext,
           filepath) {
    if (getOption("teproj.print_msg"))
      message(
        "Found ",
        action,
        " variable ",
        var,
        " on line ",
        line_idx,
        " ",
        line,
        " in script ",
        filename,
        # "(",
        # filepath,
        # ")",
        "."
      )
  }

.compile_project_io_data <-
  function(action,
           var,
           line_idx,
           line,
           filename,
           ext,
           filepath,
           accuracy,
           comment) {
    # if (getOptions("teproj.print_msg")) {
    #   if (filename == "")
    #     message("Could not identify a filename.")
    #   if (var == "")
    #     message("Could not identify a variable.")
    # }
    d <-
      tibble::tibble(
        io = action,
        var = var,
        filename = filename,
        ext = ext,
        script_line = line,
        script_line_idx = line_idx,
        script_filepath = filepath,
        accuracy = accuracy,
        comment = comment
      )
    d
  }


#' @title Parse project input/output.
#' @description Parses project to identify inputs/out variables, filenames, etc.
#' @details Intended to be used with `export_ext()*` and `import_ext*()` functions, but can work
#' if `rgx_input` and `rgx_output` are modified appropriately.
#' Does not currently work as intended if variables, filenames, etc. are not wrapped by `rgx_input` and `rgx_output`.
#' (i.e. Does not work as intended if variables, filenames, etc. is on a separate line,
#' such as with a pipe.)
#' @inheritParams create_dir
#' @param filepaths character (vector),
#' @param dir character. Should not be a vector, Only used if filepaths is missing.
#' #' @param ... dots. Parameters passed to `list.files()`. (Technically, `pattern` could/should not be specified explicitly.
#' @param rgx_file_io character. Alias to `pattern` parameter for `list.files()`. Used ONLY if `filepaths` is missing and `dir` is not.
#' @param rgx_input character. Regular expression to match for input functions,
#' @param rgx_output Chracter. Regular expression to match for out functions.,
#' @return data.frame
#' @importFrom tibble tibble as_tibble
#' @export
parse_proj_io <-
  function(filepaths,
           dir,
           ...,
           rgx_file_io = ".R",
           rgx_input = ".*(import_ext|teutils::import_ext)",
           rgx_output = ".*(export_ext|teutils::export_ext)") {
    # showConnections(all = TRUE)
    # showConnections()
    # closeAllConnections()

    # # Debugging...
    # try(close(conn), silent = TRUE)
    # filepaths = NULL
    # dir = "R/"
    # rgx_file_io = ".R"
    # rgx_input = "^(readr::import_|import_)"
    # rgx_output = "^export_"

    files_exist <- .check_files_exist(filepaths = filepaths, dir = dir, pattern = rgx_file_io, ...)
    if(!files_exist$exist)
      return(invisible())

    filepaths
    filepath_idx <- 1

    while (filepath_idx <= length(filepaths)) {
      filepath <- filepaths[filepath_idx]
      conn <- file(filepath, open = "r")
      on.exit(try(close(conn), silent = TRUE), add = TRUE)
      lines <- readLines(conn)
      line_idx <- 1

      while (line_idx <= length(lines)) {
        if (line_idx > 1) {
          line_previous <- line
        } else {
          line_previous <- ""
        }

        line <- lines[line_idx]

        # Remove comments, package calls, etc.
        line_trimmed <- gsub("(^#|\\s+#).*", "", line)
        line_trimmed <- gsub("(library|require).*", "", line_trimmed)
        line_trimmed <- gsub(".*::", "", line_trimmed)
        line_trimmed <- gsub("^\\s+|\\s+$", "", line_trimmed)

        # Assumptions that will be checked.
        match <- FALSE
        piped <- FALSE
        quoted <- FALSE
        accuracy <- "high"
        comment <- "No filename found."

        if (grepl(rgx_input, line_trimmed)) {
          # browser()
          match <- TRUE
          action <- "input"
          rgx_parse <- rgx_input

        } else if (grepl(rgx_output, line_trimmed)) {
          # browser()
          match <- TRUE
          action <- "output"
          rgx_parse <- rgx_output
        }

        if (match) {
          # browser()
          line_parsed <- gsub(rgx_parse, "", line_trimmed)
          line_parsed <- gsub("(,|\\)).*", "", line_parsed)
          line_parsed <- gsub("^_", "", line_parsed)
          var_filename <- gsub(".*\\(", "", line_parsed)
          var_filename <- gsub("\\).*", "", var_filename)
          var_filename <- gsub("\\s", "", var_filename)
          var_filename <- gsub(".*\\=", "", var_filename)
          ext <- tools::file_ext(line)
          if (var_filename == "") {
            # browser()
            piped <- TRUE
            # filename <- ""

            if(grepl("%>%", line_trimmed)) {
              line_parsed <- gsub("\\s+%>.*", "", line)
              line_parsed <- gsub(".*<-\\s+", "", line_parsed)
              var_filename <- line_parsed

              accuracy <- "medium"
              comment <- "Assuming variable is equal to expression before pipe."
            } else {
              var_filename <- gsub("\\s+%>%.*", "", line_previous)

              accuracy <- "medium"
              comment <- "Assuming variable is equal to previous line due to pipe detection."
            }
          } else {
            quoted <- grepl('\\"', var_filename)
            if (quoted) {
              # browser()
              # NOTE: import_ext*() functions are more likely to have a filename
              # than the export_ext*() functions.
              filename <- .remove_rgx(var_filename, '\\".*\\"')
              filename <- gsub('\\"', "", filename)
              var_filename <- gsub('\\".*\\"', "", var_filename)
              accuracy <- "low"
              if(length(ext) == 0) {
                ext <- ""
                # filename <- ""
                comment <- "Difficulty parsing var, filename, and ext."
              } else if(filename == ext) {
                filename <- ""
                comment <- "Difficulty parsing var and filename."
              } else {
                comment <- "Not sure if filename is correct."
              }

            } else {
              # filename <- ""
            }
          }
          filename <- basename(filepath)
          var <- var_filename

          d <-
            .compile_project_io_data(action,
                                     var,
                                     line_idx,
                                     line,
                                     filename,
                                     ext,
                                     filepath,
                                     accuracy,
                                     comment)
          .print_parse_proj_io_msg(action,
                                   var,
                                   line_idx,
                                   line,
                                   filename,
                                   ext,
                                   filepath)
          if (!exists("out")) {
            out <- d
          } else {
            out <- rbind(out, d)
          }
        }
        line_idx <- line_idx + 1
      }
      close(conn)
      filepath_idx <- filepath_idx + 1
    }
    if (!exists("out")) {
      return(invisible())
    }
    invisible(as_tibble(out))
  }
