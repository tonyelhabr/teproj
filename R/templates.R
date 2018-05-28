
#' Custom Word document template
#'
#' @description None.
#' @details None.
#' @inheritParams rmarkdown::word_document
#' @param reference_docx path. Refers to custom word document template.
#' @export
word_document_te <-
  function(toc = FALSE,
           toc_depth = 3,
           fig_width = 5,
           fig_height = 4,
           fig_caption = TRUE,
           df_print = "default",
           smart = TRUE,
           highlight = "default",
           # reference_docx = "default",
           reference_docx = system.file("rmarkdown", "templates", "te", "skeleton", "template-te.docx", package = "teproj"),
           keep_md = FALSE,
           md_extensions = NULL,
           pandoc_args = NULL) {

  rmarkdown::word_document(
    toc = FALSE,
    toc_depth = toc_depth,
    fig_width = fig_width,
    fig_height = fig_height,
    fig_caption = fig_caption,
    df_print = df_print,
    smart = smart,
    highlight = highlight,
    reference_docx = reference_docx,
    keep_md = keep_md,
    md_extensions = md_extensions,
    pandoc_args = pandoc_args
  )
  }