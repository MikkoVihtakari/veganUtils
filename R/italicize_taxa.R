#' @title Italicize species names
#' @description Italicizes species names in markdown and ggplot2 formats
#' @param x character vector of species names to be italicized
#' @param type character specifying the output type. Options: \code{"markdown"} for Rmarkdown tables, \code{"ggplot"} for ggplot axes labels and titles, and \code{"latex"} for LaTeX tables.
#' @param clean_names logical indicating whether periods and underscores in species names should be replaced by white space
#' @author Mikko Vihtakari
#' @export

# type = "markdown"; clean_names = TRUE
italicize_taxa <- function(x, type = "markdown", clean_names = TRUE) {

  if(!inherits(x, "character")) x <- as.character(x)
  x <- trimws(x)

  if(clean_names) {
    x <- gsub("\\b\\.\\b", " ", x, perl = TRUE)
    x <- gsub("\\b\\_\\b", " ", x, perl = TRUE)
  }

# Type
  chars <- switch(type,
    markdown = list(stchar = "*", midchar = " ", endchar = "*"),
    ggplot = list(stchar = "italic(", midchar = "~", endchar = ")"),
    latex = list(stchar = "\\textit{", midchar = " ", endchar = "}"),
    stop(paste("type", type, "not found")))

  y <- strsplit(x, " ")

  # k <- y[[59]]
  out <- sapply(y, function(k) {

    if(length(k) == 2) {

      if(k[2] %in% c("sp", "spp", "sp.", "spp.", "nauplii", "larvae")) {
        paste0(chars$stchar, k[1], chars$endchar, chars$midchar, k[2])
      } else if(k[2] == "indet.") {
        paste0(k, collapse = chars$midchar)
      } else if(k[1] == "Benthic") {
        paste0(k, collapse = chars$midchar)
      } else {
        paste0(chars$stchar, paste(k, collapse = chars$midchar), chars$endchar)
      }
    } else if(length(k) == 3) {
      if(k[2] %in% c("cf.", "cf. ")) {
        paste0(chars$stchar, k[1], chars$endchar, chars$midchar, k[2], chars$midchar, chars$stchar, k[3], chars$endchar)
      } else if(k[2] %in% c("sp.", "spp.", "sp", "spp")) {
        paste0(chars$stchar, k[1], chars$endchar, chars$midchar, k[2], chars$midchar, k[3])
      } else if(k[3] %in% c("symbiont", "symbionts")) {
        paste0(chars$stchar, k[1], chars$midchar, k[2], chars$endchar, chars$midchar, k[3])
      } else {
        paste0(k, collapse = chars$midchar)
      }
    } else if(length(k) == 4 & k[3] == "var.") {
        paste0(chars$stchar, k[1], chars$midchar, k[2], chars$endchar, chars$midchar, k[3], chars$midchar, chars$stchar, k[4], chars$endchar)
    } else {
      paste0(k, collapse = chars$midchar)
    }

  })

out <- trimws(out)

ifelse(out == "NA", NA, out)

}
