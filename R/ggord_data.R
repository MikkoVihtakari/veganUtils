#' @title Organize data from CCA and RDA objects for plotting in ggplot2
#' @description Internal function to extract data from vegan's \code{\link[vegan]{cca}} and \code{\link[vegan]{rda}} objects for ggplot2 ordination graphics.
#' @param mod \code{\link[vegan]{cca}} or \code{\link[vegan]{rda}} object from vegan.
#' @param env_data data frame containing environmental data.
#' @param axes numeric vector defining two ordination axes for the plot
#' @param capitalize_cn logical indicating whether centroid names should be capitalized.
#' @keywords internal
#' @author Mikko Vihtakari
#' @seealso \code{\link{ggord_plot}}
#' @export

# capitalize_cn = FALSE; axes = 1:2; env_data = z
ggord_data <- function(mod, env_data = NULL, axes = 1:2, capitalize_cn = FALSE) {

## Data ####

x <- scores(mod, tidy = TRUE)

if(is.null(mod$CCA)) {
  CONST <- FALSE
  tmp <- names(x)[grep("(CA)|(PC)", names(x))]
  names(x)[names(x) %in% tmp] <- paste0("AX", gsub("[[:alpha:]]", "", tmp))
} else {
  CONST <- TRUE
  tmp <- names(x)[grep("(CCA)|(RDA)", names(x))]
  if(length(tmp) == 1) {
    tmp <- c(tmp, names(x)[grep("(CA)|(PC)", names(x))])
    names(x)[names(x) %in% tmp] <- paste0("AX", gsub("[[:alpha:]]", "", seq_along(tmp)))
  } else {
    names(x)[names(x) %in% tmp] <- paste0("AX", gsub("[[:alpha:]]", "", tmp))
    }
}

## Select axes
x <- x[c("Score", "Label", paste0("AX", axes))]
names(x)[grepl("AX", names(x))] <- c("AX1", "AX2") # This is needed for ggplot. Scary, but should work

## Species /  columns
sp <- subset(x, Score == "species")
sp <- droplevels(sp)

## Sites / rows
st <- subset(x, Score == "sites")
st <- droplevels(st)
rownames(st) <- as.character(st$Label) #gsub("[[:alpha:]]" , "", as.character(st$Label))

if(CONST) {
  const_cols <- select(strsplit(as.character(mod$call)[2], "~"), 2)
  const_cols <- trimws(unlist(strsplit(const_cols, "\\+")))

  bp <- subset(x, Score == "biplot")
  bp <- droplevels(bp)

  if(any(levels(x$Score) %in% "centroids")) {
    CENT <- TRUE
    cn <- subset(x, Score == "centroids")
    cn <- droplevels(cn)

    cn_labs <- gsub(paste(const_cols, collapse = "|"), "", cn$Label)
    cn_cols <- unique(gsub(paste(cn_labs, collapse = "|"), "", cn$Label))

    bp <- bp[!bp$Label %in% grep(paste(cn_cols, collapse = "|"), bp$Label, value = TRUE),]
    bp <- droplevels(bp)

    cn$label <- gsub(paste(cn_cols, collapse = "|"), "", cn$Label)
    cn$variable <- gsub(paste(cn$label, collapse = "|"), "", cn$Label)

    if(nrow(bp) > 0) BPARR <- TRUE else BPARR <- FALSE
  }

} else {
  bp <- NULL
  cn <- NULL
  CENT <- FALSE
  BPARR <- FALSE
}

if(capitalize_cn & CONST) {

  # i <- 1
  if(CENT) {
    levels(cn$Label) <- sapply(seq_along(levels(cn$Label)), function(i) {

    k <- gsub(paste(const_cols, collapse = "|"), "", levels(cn$Label)[i])

    if(k == "") {
      levels(cn$Label)[i]
    } else {
      k
    }
  })
}

  if(BPARR) levels(bp$Label) <- Hmisc::capitalize(levels(bp$Label))
  levels(cn$Label) <- Hmisc::capitalize(levels(cn$Label))
}

## Environmental data ####

if(is.null(env_data)) {
  ENV <- FALSE
} else {
 if(any(gsub("\\D", "", rownames(st)) != rownames(env_data))) stop("rownames of sites and env_data do not match.")
  st <- cbind(st, env_data)
  ENV <- TRUE
}

## Export ####

out <- list(sp = sp, st = st, cn = cn, bp = bp, defs = c("CONST" = CONST, "CENT" = CENT, "BPARR" = BPARR, "ENV" = ENV))

class(out) <- "ggordData"

out
## ####

}
