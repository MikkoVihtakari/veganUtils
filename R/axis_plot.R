#' @title Lollipop chart of species contributions to ordination axes (ggplot2)
#' @description Creates a lollipop chart of species contributions to ordination axes with scalable point size
#' @param mod \code{\link[vegan]{cca}} or rda object
#' @param axis The ordination axis for which the chart should be made for
#' @param n Number of columns (=species) to show
#' @param sides Character vector specifying which columns should be shown. Options: \code{"both"} shows \code{n/2} from both sides of the axis ordered by species scores; \code{"contrib"} orders the species based on "contributions" to the axis (squared species scores); \code{pos} orders the species based on the species score to the axis and \code{"neg"} orders the species based on the inverted species score to the axis.
#' @param size_preset Character. Define the size preset of the plot. Options: \code{"device"}, which is optimized for the R graph window, and \code{"pdf"}, which is optimized as subplots to go together with \code{\link{ggord_plot}}.
#' @param species A character vector of species entries to be forced to the plot. The remaining of species will be taken from \code{sides} and \code{n} arguments.
#' @param point_cols a character vector of length 2 specifying the colors for negative and positive species scores respectively.
#' @param xlab X-axis label
#' @param ylab Y-axis label. If \code{NULL}, the label is generated automatically from the \code{\link[vegan]{cca}} or rda object.
#' @param sp_names Optional data frame specifying the species names to be used in the plot. The first column has to specify the species names used in the \code{\link[vegan]{cca}} or rda object and the second column the species names that should be used to replace the original names.
#' @param italicize_sp_names Logical. Should species names be italicized?
#' @param point_size Single number specifying the size for "lollipop heads".
#' @param text_col Character. Color of text for the points. The text spcifies species contributions to the axis in percentages.
#' @param map_size Logical. Should point size be mapped to species contributions to the axis?
#' @param break_interval Single number specifying the interval to be used for x-axis.
#' @import ggplot2 vegan
#' @importFrom plyr mapvalues
#' @importFrom plyr round_any
#' @author Mikko Vihtakari
#' @examples library(vegan)
#' data(dune)
#' axis_plot(rda(dune), break_interval = 0.5)
#' @export

#library(tidyverse)
# mod <- mod1c; axis = 1; n = 10; sides = "contrib"; point_cols = c("#f8766d", "#00ba38"); xlab = NULL; ylab = NULL; sp_names = sp_list; text_col = "white"; map_size = TRUE; point_size = 6; break_interval = 0.5; size_preset = "device"; species = c("Cfin", "Cgla")

axis_plot <- function(mod, axis = 1, sides = "both", n = 10, species = NULL, size_preset = "device", point_cols = c("#f8766d", "#00ba38"), xlab = NULL, ylab = NULL, sp_names = NULL, italicize_sp_names = FALSE, point_size = NULL, text_col = "white", map_size = FALSE, break_interval = NULL) {

  ## ####

  tmp <- extract_species(mod = mod, axis = axis, sides = sides, species = species, n = n)

  contrib <- 100*scores(mod, scaling = 0, display = "sp")[,axis]^2

  xp <- data.frame(variable = names(tmp), value = unname(tmp))
  xp$variable <- factor(xp$variable, levels = xp$variable)
  xp$sign <- ifelse(sign(xp$value) < 0, "neg", "pos")
  xp <- merge(xp, data.frame(variable = names(contrib), contr = unname(contrib)), all.x = TRUE, sort = FALSE)

  if(is.null(xlab)) {
    xlab <- paste(names(axis_expl(mod)[axis]), "value")
  }

  if(!is.null(sp_names)) {
    levels(xp$variable) <- plyr::mapvalues(levels(xp$variable), as.character(sp_names[[1]]), as.character(sp_names[[2]]), warn_missing = FALSE)
  }

  if(italicize_sp_names) {
    levels(xp$variable) <- italicize_taxa(levels(xp$variable), type = "ggplot")
  }

xmin <- plyr::round_any(min(xp$value), 0.1, floor)
xmax <- plyr::round_any(max(xp$value), 0.1, ceiling)

if(!is.null(break_interval)) {
  neg.breaks <- seq(0, (xmin-2*break_interval), -break_interval)
  pos.breaks <- seq(0, (xmax+2*break_interval), break_interval)
  breaks <- sort(c(neg.breaks, pos.breaks), decreasing = TRUE)
  breaks <- breaks[!duplicated(breaks)]
}


if(size_preset == "pdf" & is.null(point_size)) {
 sizes <- list(base_size = 6, point_size = 4, top = 0.5, right = 0.2, bottom = 0.5, left = 0.1, e1 = 0.05, e2 = 0.1)
} else {
 sizes <- list(base_size = 8, point_size = 6, top = 0.5, right = 0.2, bottom = 0.5, left = 0.1, e1 = 0.05, e2 = 0)
}


  ## ####
  out <- ggplot(data = xp, aes(x = variable, y = value, fill = sign, label = round(contr, 1))) +
    geom_hline(yintercept = 0, size = LS(0.5), color = "grey50") +
    geom_segment(aes(y = 0, x = variable, yend = value, xend = variable), size = LS(0.5)) +
    theme_minimal(base_size = sizes$base_size) +
    theme(legend.position = "none",
      axis.line.x = element_line(size = LS(0.5), color = "black"),
      axis.ticks.x = element_line(size = LS(0.5), color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(sizes$top, sizes$right, sizes$bottom, sizes$left), units = "line"))
  ## ####

if(italicize_sp_names) {
  out <- out +
   scale_x_discrete(name = ylab, labels = unlist(label_parsed(as.character(xp$variable))))
} else {
  out <- out + xlab(ylab)
}

  if(is.null(break_interval)) {
    out <- out +
      coord_flip() +
      scale_y_continuous(xlab, expand = c(sizes$e1, sizes$e2))
  } else {
    out <- out +
      coord_flip(ylim = c(xmin, xmax)) +
      scale_y_continuous(name = xlab, breaks = breaks, expand = c(sizes$e1, sizes$e2))
  }

  if(map_size) {
    # ####
    out +
      geom_point(aes(size = contr), shape = 21, stroke = LS(0.5)) +
      geom_text(color= text_col, size = 1/3*sizes$point_size) +
      scale_fill_manual(values = point_cols) +
      scale_size(range = c(sizes$point_size-1, sizes$point_size+4))
    # ####
  } else {
    # ####
    out +
      geom_point(size = sizes$point_size, shape = 21, stroke = LS(0.5)) +
      geom_text(color= text_col, size = 1/3*sizes$point_size) +
      scale_fill_manual(values = point_cols)
    # ####
  }

}
