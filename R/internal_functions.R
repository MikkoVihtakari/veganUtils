#' @title Convert font sizes measured as points to ggplot font sizes
#' @description Converts font sizes measured as points (as given by most programs such as MS Word etc.) to ggplot font sizes
#' @param x numeric vector giving the font sizes in points
#' @return Returns a numeric vector of lenght \code{x} of ggplot font sizes
#' @keywords internal
FS <- function(x) x/2.845276 # x is the desired font / line size in pt

#' @title Convert line sizes measured as points to ggplot line sizes
#' @description Converts line sizes measured as points (as given by most programs such as Adobe Illustrator etc.) to ggplot font sizes
#' @param x numeric vector giving the lines sizes in points
#' @return Returns a numeric vector of lenght \code{x} of ggplot line sizes
#' @keywords internal
LS <- function(x) x/2.13

#' @title Internal helper function to extract most contributing species from vegan's cca and rda objects
#' @description Internal helper function to extract most contributing species from vegan's cca and rda objects
#' @param mod \code{\link[vegan]{cca}} or rda object
#' @param axis The ordination axis for which the extraction should be made for
#' @param sides Character vector specifying which columns should be shown. Options: \code{"both"} shows \code{n/2} from both sides of the axis ordered by species scores; \code{"contrib"} orders the species based on "contributions" to the axis (squared species scores); \code{pos} orders the species based on the species score to the axis and \code{"neg"} orders the species based on the inverted species score to the axis.
#' @param species A character vector of species entries to be forced to the plot. The remaining of species will be taken from \code{sides} and \code{n} arguments.
#' @param n Number of columns (=species) to show
#' @keywords internal
#' @author Mikko Vihtakari

extract_species <- function(mod, axis, sides, species, n) {
  tmp <- scores(mod, display = "sp")[,axis]
  contrib <- 100*scores(mod, scaling = 0, display = "sp")[,axis]^2

   if(!all(species %in% names(tmp))) stop("species could not be found from the mod object. Check the species argument")

   if(sides == "contrib") {
    tmp <- tmp[c(species, names(sort(contrib, decreasing = TRUE)))[1:n]]
    tmp <- tmp[order(-tmp)]
    tmp <- tmp[!duplicated(tmp)]
  } else if(sides == "-contrib") {
    tmp <- tmp[c(species, names(sort(contrib, decreasing = TRUE)))[1:n]]
    tmp <- tmp[order(tmp)]
    tmp <- tmp[!duplicated(tmp)]
  } else if(sides == "pos") {
    tmp <- tmp[c(species, names(sort(tmp, decreasing = TRUE)))[1:n]]
    tmp <- tmp[order(-tmp)]
    tmp <- tmp[!duplicated(tmp)]
  } else if(sides == "neg") {
    tmp <- tmp[c(species, names(sort(tmp, decreasing = FALSE)))[1:n]]
    tmp <- tmp[order(tmp)]
    tmp <- tmp[!duplicated(tmp)]
  } else {
    sp1 <- tmp[c(species, names(sort(tmp, decreasing = TRUE)))[1:(ceiling(n/2)+ceiling(length(species)/2))]]
    sp2 <- tmp[c(species, names(sort(tmp, decreasing = FALSE)))[1:(ceiling(n/2)+ceiling(length(species)/2))]]
    tmp <- c(sp1, sp2)
    tmp <- tmp[order(tmp)]
    tmp <- tmp[!duplicated(tmp)]
  }

  tmp
}

#' @title Select an element of each vector from a list
#' @description Selects y'th element of each vector from a list
#' @param x list
#' @param y number of element. Must be integer
#' @keywords internal

select <- function(x,y) sapply(x, "[", y)
