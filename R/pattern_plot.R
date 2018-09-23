#' @title Facet grid to examine patterns in multivariate data (ggplot2)
#' @description ggplot2 based facet_grid to examine patterns in multivariate data
#' @param mod \code{\link[vegan]{cca}} or \code{\link[vegan]{rda}} object from vegan.
#' @param sides Character vector specifying which columns should be shown. Options: \code{"both"} shows \code{n/2} from both sides of the axis ordered by species scores; \code{"contrib"} orders the species based on "contributions" to the axis (squared species scores); \code{pos} orders the species based on the species score to the axis and \code{"neg"} orders the species based on the inverted species score to the axis.
#' @param species character vector specifying the species names that should be forced to the plot. Any missing species will be taken from the \code{n_sps} argument. If \code{NULL} all species will be taken from the \code{n_sps} argument.
#' @param n integer giving the number of species that should be plotted. Species are chosen based on contribution to \code{axis}
#' @param axis ordination axis, which should be used for the plot
#' @param expls character vector specifying the environmental/explanatory variable names that should be used in the plot. If \code{NULL} and constrained model, variables will be taken from \code{mod}.
#' @param log_trans logical indicating whether species values should be logarithm + 1 transformed before plotting.
#' @param X optional species data frame. If \code{NULL}, the same data frame than in \code{mod} will be used.
#' @param Y optional constraining variable data frame. If \code{NULL}, the same data frame than in \code{mod} will be used.
#' @import ggplot2 vegan reshape2
#' @author Mikko Vihtakari
#' @export

# mod = mod1cl; sps = c("Cfin", "Cgla", "METlon", "Paraeu", "BIVALV", "Microc", "COPEPO"); expls = c("year", "temp", "depth"); log.trans = FALSE; axis = 1; n_sps = 7
# axis = 1; sides = "contrib"; n = 7; species = NULL; expls = NULL; log_trans = FALSE; X = NULL; Y = NULL


pattern_plot <- function(mod, axis = 1, sides = "both", n = 10, species = NULL, expls = NULL, log_trans = FALSE, X = NULL, Y = NULL) {

  sps <- names(extract_species(mod = mod, axis = axis, sides = sides, species = species, n = n))

  if(is.null(expls)) {
    expls <- trimws(unlist(strsplit(unlist(strsplit(as.character(mod$call)[2], "~"))[2], "\\+")))
  }

  if(is.null(X)) {
    sp.mat <- trimws(unlist(strsplit(as.character(mod$call)[2], "~"))[1])
    sp.mat <- get(sp.mat)
  } else {
    sp.mat <- X
  }

  if(is.null(Y)) {
    exp.dt <- trimws(as.character(mod$call)[3])
    exp.dt <- get(exp.dt)
  } else {
    exp.dt <- Y
  }

z <- cbind(sp.mat[sps], exp.dt[expls])
z$id <- rownames(z)
z <- reshape2::melt(z, id = c("id", expls), variable.name = "sp", value.name = "sp.value")

if(log_trans) z$sp.value <- log(z$sp.value + 1)

# k <- expls[1]
plots <- lapply(seq_along(expls), function(i) {

  if(i == 1) first <- TRUE else first <- FALSE
  if(i == length(expls)) last <- TRUE else last <- FALSE

  p <- ggplot(z, aes(x = get(expls[i]), y = sp.value)) +
  ggtitle(expls[i]) +
  theme_bw(base_size = 8) +
  ylab("Species value") +
  xlab(NULL) +
  facet_grid(sp~., scales = "free")

  if(!first) {
    p <- p + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  }

  if(!last) {
    p <- p + theme(strip.text = element_blank())
  }

  if(any(class(z[[expls[i]]]) %in% c("factor", "character"))) {
    p + geom_boxplot(size = LS(0.5), outlier.size = 1, outlier.shape = 21, outlier.stroke = LS(0.5))
  } else {
    p + geom_smooth(se = FALSE) + geom_point(size = 1, alpha = 0.3)
  }
})

do.call("grid.arrange", c(plots, nrow=1))
}

