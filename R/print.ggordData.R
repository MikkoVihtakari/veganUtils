##' @title Print \code{ggordData} objects
##' @description \code{\link{print}} function for \code{\link[=ggord_plot]{ggordData}} objects
##' @param x \code{ggordData} object to be printed.
##' @param ... further arguments passed to \code{\link{print}}.
##' @method print ggordData
##' @export
##' @importFrom utils head object.size tail
##' @author Mikko Vihtakari

print.ggordData <- function(x, ...) {

  cat("ggordData object", sep = "\n")
  cat("A list of data containing following elements:", sep = "\n")
  cat(names(x), sep = ", ")
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  
  cat(paste0("File size ", format(utils::object.size(x), units = "Kb")), sep = "\n")
  cat(NULL, sep = "\n")
  
  cat("Species (column) scores (use $sp to index):", sep = "\n")
  cat(paste0(nrow(x$sp), " rows. ", ncol(x$sp), " columns."), sep = "\n")
  cat("Column names: ")
  cat(colnames(x$sp), sep = ", ")
  cat(NULL, sep = "\n")
  cat("Species (use $sp$Label to index): ")
  cat(utils::head(as.character(x$sp$Label), 3), "...", utils::tail(as.character(x$sp$Label), 3), sep = ", ")
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  
  cat("Site (row) scores (use $st to index):", sep = "\n")
  cat(paste0(nrow(x$st), " rows. ", ncol(x$st), " columns."), sep = "\n")
  cat(NULL, sep = "\n")
  
  if(!x$defs[names(x$defs) == "ENV"]) {
    cat("No environmental data", sep = "\n")
  } else {
    cat("Environmental data cbind to site scores.", sep = "\n")
  }
  
  cat(NULL, sep = "\n")
  
  if(is.null(x$cn)) {
    cat("No centroid scores", sep = "\n")
  } else {
    cat("Centroid scores (use $cn to index):", sep = "\n")
    cat(paste0(nrow(x$cn), " rows. ", ncol(x$cn), " columns."), sep = "\n")
    cat("Centroid labels: ")
    cat(unique(as.character(x$cn$Label)), sep = ", ")
    cat(NULL, sep = "\n") 
  }
  
  cat(NULL, sep = "\n")
  
  if(is.null(x$bp)) {
    cat("No biplot arrows", sep = "\n")
  } else {
    cat("Biplot arrows (use $bp to index):", sep = "\n")
    cat(paste0(nrow(x$bp), " rows. ", ncol(x$bp), " columns."), sep = "\n")
    cat("Biplot labels: ")
    cat(unique(as.character(x$bp$Label)), sep = ", ")
    cat(NULL, sep = "\n")
  }
  
  cat(NULL, sep = "\n")
  
  cat("Definitions (use $defs to index):", sep = "\n")
  cat(paste(names(x$defs), x$defs, sep = " = "), sep = ", ")
  }
