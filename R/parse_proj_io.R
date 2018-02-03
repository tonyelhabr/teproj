

.get_valid_exts <- function(action = c("input", "output")) {
  requireNamespace("rio")
  action <- match.arg(action)
  rgx_pattern_0 <-  "rio|(\\.)|_"
  rgx_pattern_suffix <- switch(action, input = "import", output = "export")
  # browser()
  rgx_pattern <- paste0(rgx_pattern_0, "|", rgx_pattern_suffix)
  if(action == "input") {
    output <- gsub(rgx_pattern, "", as.character(utils::methods(.import)))
  } else if (action == "output") {
    output <- gsub(rgx_pattern, "", as.character(utils::methods(.export)))
    output <- c(output, "png")
  }
  output
}

# See: https://stackoverflow.com/questions/2192316/extract-a-regular-expression-match-in-r-version-2-10.
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
    if (getOption("teproj.print.msg"))
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
    # message(sprintf("Found %s variable %s on line %.0f in script %s.", action, var, line_idx, filename))
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
    # if (getOptions("teproj.print.msg")) {
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
        line = line,
        line_idx = line_idx,
        filepath = filepath,
        accuracy = accuracy,
        comment = comment
      )
    d
  }


#' Parse input and output in project scripts
#'
#' @description Parses project diretory to identify input andoutput variables, filenames, etc.
#' @details Intended to be used with this package's \code{export()} and \code{import()} functions, but it can also work
#' with the functions from other packages (e.g. \code{readr}) if \code{rgx_input} and \code{rgx_output} are modified appropriately.
#' For example, \code{rgx_input == "read"} and \code{rgx_output == "write"} might be appropriate choices if using the \code{readr} package.)
#' Does not (currently) work as well if variables, filenames, etc. are not wrapped directly by
#' the functions described by \code{rgx_input} and \code{rgx_output}.
#' NOTE: In the future this should/will use \code{utils::getParseData(parse(f))}
#' @inheritParams create_dir
#' @param filepaths character. Can be a vector. Missing by default. Takes precedence over \code{dir} argument.
#' @param dir character. Should not be a vector. Missing by default. Only used if \code{filepaths} is missing.
#' @param ... dots. Arguments passed to \code{list.files()} for identifying input/output files.
#'(Technically, \code{pattern} could/should not be specified explicitly.)
#' @param rgx_file_io character. Alias to \code{pattern} parameter for \code{list.files()}.
#' Used ONLY if \code{filepaths} is missing and \code{dir} is not.
#' @param rgx_input character. Regular expression to match for input functions
#' @param rgx_output Chracter. Regular expression to match for output functions.
#' @return data.frame
#' @importFrom tibble tibble as_tibble
#' @importFrom utils methods
#' @import rio
#' @export
parse_proj_io <-
  function(filepaths,
           dir,
           ...,
           rgx_file_io = ".R",
           rgx_input = ".*(import_ext|read)",
           rgx_output = ".*export_ext|write)") {
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

    files_exist <-
      .check_files_exist(filepaths = filepaths,
                         dir = dir,
                         pattern = rgx_file_io,
                         ...)
    if (!files_exist$exist) {
      return(invisible())
    } else {
      filepaths <- files_exist$filepaths
    }

    # browser()
    # filepaths <- normalizePath(filepaths)
    filepaths <- .normalize_path(filepaths, mustWork = FALSE)

    filepath_idx <- 1

    while (filepath_idx <= length(filepaths)) {
      filepath <- filepaths[filepath_idx]
      # TODO:utils::getParseData(parse(filepath)) here.
      # browser()

      conn <- file(filepath, open = "r")
      on.exit(try(close(conn), silent = TRUE)
              , add = TRUE)
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
        line_trimmed <-
          gsub("(library|require).*", "", line_trimmed)
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

          exts_valid <- .get_valid_exts(action)
          # browser()
          exts_valid_collapsed <- paste(paste0("(", exts_valid, ")"), collapse = "|")
          ext <- .remove_rgx(line_parsed, exts_valid_collapsed)
          if(length(ext) == 0) ext <- ""

          if (var_filename == "") {
            # browser()
            piped <- TRUE
            # filename <- ""

            if (grepl("%>%", line_trimmed)) {
              # browser()
              line_parsed <- gsub("\\s+%>.*", "", line)
              line_parsed <- gsub(".*<-\\s+", "", line_parsed)
              var_filename <- line_parsed

              accuracy <- "medium"
              comment <-
                "Assuming variable is equal to expression before pipe."
            } else {
              var_filename <- gsub("\\s+%>%.*", "", line_previous)

              accuracy <- "medium"
              comment <-
                "Assuming variable is equal to previous line due to pipe detection."
            }
          } else {
            quoted <- grepl('\\"', var_filename)
            if (quoted) {
              # browser()
              # NOTE: import_ext*() functions are more likely to have a filename
              # than the export_ext*() functions.
              filename <- .remove_rgx(var_filename, '\\".*\\"')
              filename <- gsub('\\"', "", filename)
              if(length(filename) == 0) filename <- ""
              var_filename <- gsub('\\".*\\"', "", var_filename)
              accuracy <- "low"
              if (ext == "") {

                # filename <- ""
                comment <-
                  "Difficulty parsing var, filename, and ext."
              } else if (!(filename == "") & filename == ext) {
                # browser()
                filename <- ""
                comment <- "Difficulty parsing var and filename."
              } else {
                comment <- "Difficulty parsing filename"
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
                                     line_trimmed,
                                     filename,
                                     ext,
                                     filepath,
                                     accuracy,
                                     comment)
          tibble::tibble(
            io = action,
            # var = var,
            # filename = "",
            ext = ext,
            script_line = line,
            script_line_idx = line_idx,
            script_filepath = filepath,
            accuracy = accuracy,
            comment = comment
          )
          .print_parse_proj_io_msg(action,
                                   var,
                                   line_idx,
                                   line,
                                   filename,
                                   ext,
                                   filepath)
          # browser()
          if (!exists("out", inherits = FALSE)) {
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
    if (!exists("out", inherits = FALSE)) {
      return(invisible())
    }
    invisible(as_tibble(out))
  }
