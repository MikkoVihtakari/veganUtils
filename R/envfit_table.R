#' @title Produce a data frame from an envfit object
#' @description Produces an organized data frame from an envfit object
#' @param ord An ordination object or other structure from which the ordination scores can be extracted (including a data frame or matrix of scores). See \code{\link[vegan]{envfit}}.
#' @param env Data frame, matrix or vector of environmental variables. The variables can be of mixed type (factors, continuous variables) in data frames. See \code{\link[vegan]{envfit}}.
#' @param round_digits integer giving the number of digits R2 and PC values should be rounded to.
#' @param ... other arguments to be passed to  \code{\link[vegan]{envfit}}
#' @param clean_var_names logical indicating whether variable names should be tidied up. Useful for exporting the table to publications.
#' @return Returns a data frame
#' @import vegan tibble
#' @importFrom Hmisc capitalize
#' @importFrom plyr rbind.fill
#' @importFrom gridExtra grid.arrange
#' @author Mikko Vihtakari
#' @export

# test parameters
# ord = mod; env = y; r2.type = "adj"; round_digits = 2; clean_var_names = FALSE
envfit_table <- function(ord, env, round_digits = 2, clean_var_names = FALSE, ...) {

x <- suppressWarnings(envfit(ord, env, ...))

tabf <- data.frame(Type = "Factor", R2 = round(x$factors$r, round_digits), p = x$factors$pvals)
tabf <- tibble::add_column(tabf, Variable = rownames(tabf), .before = 1)

tabv <- data.frame(Type = "Vector", R2 = round(x$vectors$r, round_digits), p = x$vectors$pvals, PC1 = round(x$vectors$arrows[,1], round_digits), PC2 = round(x$vectors$arrows[,2], round_digits))
tabv <- tibble::add_column(tabv, Variable = rownames(tabv), .before = 1)

tab <- plyr::rbind.fill(tabf, tabv)

tab <- tab[c("Type", "Variable", "R2", "p", "PC1", "PC2")]

tab <- tab[with(tab, order(Type, -R2)),]
rownames(tab) <- 1:nrow(tab)

if(clean_var_names) {
  tab$Variable <- gsub("_", " ", Hmisc::capitalize(tab$Variable))
  tab$Variable <- gsub("\\.", " ", tab$Variable)
  tab$Variable <- gsub("\\n", " ", tab$Variable)
  
  if(any(tab$Variable %in% "Temp")) tab$Variable[tab$Variable == "Temp"] <- "Temperature"
  if(any(tab$Variable %in% "Sal")) tab$Variable[tab$Variable == "Sal"] <- "Salinity"
}

tab
}
