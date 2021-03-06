% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggord_data.R
\name{ggord_data}
\alias{ggord_data}
\title{Organize data from CCA and RDA objects for plotting in ggplot2}
\usage{
ggord_data(mod, env_data = NULL, axes = 1:2, capitalize_cn = FALSE)
}
\arguments{
\item{mod}{\code{\link[vegan]{cca}} or \code{\link[vegan]{rda}} object from vegan.}

\item{env_data}{data frame containing environmental data.}

\item{axes}{numeric vector defining two ordination axes for the plot}

\item{capitalize_cn}{logical indicating whether centroid names should be capitalized.}
}
\description{
Internal function to extract data from vegan's \code{\link[vegan]{cca}} and \code{\link[vegan]{rda}} objects for ggplot2 ordination graphics.
}
\seealso{
\code{\link{ggord_plot}}
}
\author{
Mikko Vihtakari
}
\keyword{internal}
