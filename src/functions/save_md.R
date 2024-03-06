
#' Save R Markdown (.Rmd) files as .md
#'
#' @param file input .Rmd file
#'
#' @examples
#' save_md(file = "./src/sandbox/readme.Rmd")


save_md <- function(file) {

  rmarkdown::render(input = file,
                    output_format = rmarkdown::md_document())

  cat(paste0(" \nSaved file locally as '",
             gsub(".Rmd", ".md", file),
             "'.\n"))

}
