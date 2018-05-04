


get_valid_exts <- function(action = c("input", "output")) {
  requireNamespace("rio")
  action <- match.arg(action)
  rgx_pattern_0 <-  "rio|(\\.)|_"
  rgx_pattern_suffix <-
    switch(action, input = "import", output = "export")
  # browser()
  rgx_pattern <- paste0(rgx_pattern_0, "|", rgx_pattern_suffix)
  if (action == "input") {
    output <-
      gsub(rgx_pattern, "", as.character(utils::methods(.import)))
  } else if (action == "output") {
    output <-
      gsub(rgx_pattern, "", as.character(utils::methods(.export)))
    output <- c(output, "png")
  }
  output
}

# See: https://stackoverflow.com/questions/2192316/extract-a-regular-expression-match-in-r-version-2-10.
remove_rgx <- function(char = NULL, rgx = NULL) {
  regmatches(char, regexpr(rgx, char))
}

print_parse_proj_io_msg <-
  function(action = NULL,
           var = NULL,
           line_idx = NULL,
           line = NULL,
           file = NULL,
           ext = NULL,
           path = NULL) {
    message(
      sprintf(
        "Found %s variable %s on line %s %s in script %s.",
        action,
        var,
        as.character(line_idx),
        line,
        file
      )
    )
  }

compile_project_io_data <-
  function(action = NULL,
           var = NULL,
           line_idx = NULL,
           line = NULL,
           file = NULL,
           ext = NULL,
           path = NULL,
           accuracy = NULL,
           comment = NULL) {
    tibble::tibble(
      io = action,
      var = var,
      file = file,
      ext = ext,
      line = line,
      line_idx = line_idx,
      path = path,
      accuracy = accuracy,
      comment = comment
    )
  }


#' Parse input and output in project scripts
#'
#' @description Parses project diretory to identify input andoutput variables, basenames, etc.
#' @details Intended to be used with this package's \code{export()} and \code{import()} functions, but it can also work
#' with the functions from other packages (e.g. \code{readr}) if \code{rgx_input} and \code{rgx_output} are modified appropriately.
#' For example, \code{rgx_input == "read"} and \code{rgx_output == "write"} might be appropriate choices if using the \code{readr} package.)
#' Does not (currently) work as well if variables, basenames, etc. are not wrapped directly by
#' the functions described by \code{rgx_input} and \code{rgx_output}.
#' NOTE: In the future this should/will use \code{utils::getParseData(parse(f))}
#' @inheritParams create_dir
#' @param paths character. Can be a vector. Missing by default. Takes precedence over \code{dir} argument.
#' @param dir character. Should not be a vector. Missing by default. Only used if \code{paths} is missing.
#' @param ... dots. Arguments passed to \code{list.files()} for identifying input/output files.
#'(Technically, \code{pattern} could/should not be specified explicitly.)
#' @param rgx_file_io character. Alias to \code{pattern} parameter for \code{list.files()}.
#' Used ONLY if \code{paths} is missing and \code{dir} is not.
#' @param rgx_input character. Regular expression to match for input functions
#' @param rgx_output character. Regular expression to match for output functions.
#' @return data.frame.
#' @importFrom tibble tibble as_tibble
#' @importFrom utils methods
#' @import rio
# #' @export
parse_proj_io <-
  function(paths,
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
    # paths = NULL
    # dir = "R/"
    # rgx_file_io = ".R"
    # rgx_input = "^(readr::import_|import_)"
    # rgx_output = "^export_"

    files_exist <-
      check_files_exist(paths = paths,
                        dir = dir,
                        pattern = rgx_file_io,
                        ...)
    if (!files_exist$exist) {
      return(invisible())
    } else {
      paths <- files_exist$paths
    }

    # browser()
    # paths <- normalizePath(paths)
    paths <- normalize_path(paths, mustWork = FALSE)

    path_idx <- 1

    while (path_idx <= length(paths)) {
      path <- paths[path_idx]
      # TODO: Try using utils::getParseData(parse(path)) here.
      # browser()

      conn <- file(path, open = "r")
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
        comment <- "No file found."

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
          var_basename <- gsub(".*\\(", "", line_parsed)
          var_basename <- gsub("\\).*", "", var_basename)
          var_basename <- gsub("\\s", "", var_basename)
          var_basename <- gsub(".*\\=", "", var_basename)

          exts_valid <- get_valid_exts(action)
          # browser()
          exts_valid_collapsed <-
            paste(paste0("(", exts_valid, ")"), collapse = "|")
          ext <- remove_rgx(line_parsed, exts_valid_collapsed)
          if (length(ext) == 0)
            ext <- ""

          if (var_basename == "") {
            # browser()
            piped <- TRUE
            # file <- ""

            if (grepl("%>%", line_trimmed)) {
              # browser()
              line_parsed <- gsub("\\s+%>.*", "", line)
              line_parsed <- gsub(".*<-\\s+", "", line_parsed)
              var_basename <- line_parsed

              accuracy <- "medium"
              comment <-
                "Assuming variable is equal to expression before pipe."
            } else {
              var_basename <- gsub("\\s+%>%.*", "", line_previous)

              accuracy <- "medium"
              comment <-
                "Assuming variable is equal to previous line due to pipe detection."
            }
          } else {
            quoted <- grepl('\\"', var_basename)
            if (quoted) {
              # browser()
              # NOTE: import_ext*() functions are more likely to have a file
              # than the export_ext*() functions.
              file <- remove_rgx(var_basename, '\\".*\\"')
              file <- gsub('\\"', "", file)
              if (length(file) == 0)
                file <- ""
              var_basename <- gsub('\\".*\\"', "", var_basename)
              accuracy <- "low"
              if (ext == "") {
                # file <- ""
                comment <-
                  "Difficulty parsing var, file, and ext."
              } else if (!(file == "") & file == ext) {
                # browser()
                file <- ""
                comment <- "Difficulty parsing var and file."
              } else {
                comment <- "Difficulty parsing file"
              }

            } else {
              # file <- ""
            }
          }
          file <- file(path)
          var <- var_basename

          d <-
            compile_project_io_data(action,
                                    var,
                                    line_idx,
                                    line_trimmed,
                                    file,
                                    ext,
                                    path,
                                    accuracy,
                                    comment)
          tibble::tibble(
            io = action,
            # var = var,
            # file = "",
            ext = ext,
            script_line = line,
            script_line_idx = line_idx,
            script_path = path,
            accuracy = accuracy,
            comment = comment
          )
          print_parse_proj_io_msg(action,
                                  var,
                                  line_idx,
                                  line,
                                  file,
                                  ext,
                                  path)
          # browser()
          if (!exists("ret", inherits = FALSE)) {
            ret <- d
          } else {
            ret <- rbind(ret, d)
          }
        }
        line_idx <- line_idx + 1
      }
      close(conn)
      path_idx <- path_idx + 1
    }
    if (!exists("ret", inherits = FALSE)) {
      return(invisible())
    }
    invisible(as_tibble(ret))
  }
