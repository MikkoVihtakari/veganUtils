#' @title Get percent of total inertia explained by principal or constrained axes
#' @param mod cca.object
#' @param axes A numeric vector indicating the axes for which percent explained inertia should be returned for
#' @return Returns a named vector containing explained inertia as percentages. Returns constrained inertia fo CCA and unconstrained for CA
#' @import vegan
#' @author Mikko Vihtakari
#' @export

axis_expl <- function(mod, axes = 1:2) {

  if(is.null(mod$CCA)) {
    sapply(axes, function(i) {
    100*mod$CA$eig[i]/mod$tot.chi
    })
  } else {
    sapply(axes, function(i) {
    out <- 100*mod$CCA$eig[i]/mod$tot.chi
      if(is.na(out)) {
      100*mod$CA$eig[i-1]/mod$tot.chi
      } else {
        out
      }

    })
  }

}
