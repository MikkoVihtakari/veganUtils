#' @title Alternative ordination plot for CCA and RDA objects (ggplot2)
#' @description Alternative to plot vegan's CCA and RDA objects using ggplot2. Produces illustrative and good looking graphics with less code and need for customization. Plots \code{\link[vegan]{envfit}} and \code{\link[vegan]{ordisurf}} when needed.
#' @param mod \code{\link[vegan]{cca}} or \code{\link[vegan]{rda}} object from vegan.
#' @param env_data optional data frame containing environmental data, which should be fitted using the \code{\link[vegan]{envfit}} and/or \code{\link[vegan]{ordisurf}} functions. The data frame must have the same row order and number than the species data frame used to construct \code{mod}.
#' @param size_preset character specifying the size preset for the graphic device. Options: "device" or "pdf". Size of symbols in the plot will be automatically scaled to the device or 122 mm wide pdf (page width for many scientific journals), respectively. You can override the scaling using the \code{*_size} arguments.
#' @param arrow.mul factor to expand arrows in the graph. Analogous to the \code{\link[vegan]{plot.cca}} argument of similar name.
#' @param ordisurf_var character specifying the column name in \code{env_data} that should be used to plot \code{\link[vegan]{ordisurf}}. If \code{NULL}, no \code{\link[vegan]{ordisurf}} will be plotted.
#' @param centroids logical indicating whether CCA or RDA centroids should be plotted (\code{display = "cn"} in \code{\link[vegan]{plot.cca}}).
#' @param envfits character vector specifying the names of \code{\link[vegan]{envfit}} objects, which should be plotted.
#' @param sp_repel_method character specifying desired the method to avoid cluttering of species names. Options: "orditorp", "label_omit", "ggrepel" or \code{NULL}. "orditorp" uses the approach from \code{\link[vegan]{orditorp}} to select most contributing species labels, "label_omit" uses the \code{sp_label_omit} argument to omit species names, "ggrepel" uses the \code{\link[ggrepel]{geom_text_repel}} function together with the \code{sp_label_omit} argument and \code{NULL} does not omit any species names. All omited species names are illustrated using \code{+} symbols.
#' @param sp_label_omit numeric value giving the boundary for omiting species names. Any species labels less than this argument on either axis will be replaced by \code{+} symbols. Use \code{sp_repel_method} to disable the omiting.
#' @param capitalize_cn logical indicating whether centroid names should be capitalized.
#' @param site_col character specifying the color for sites (\code{display = "sites"} in \code{\link[vegan]{plot.cca}}).
#' @param sp_col character specifying the color for species (\code{display = "species"} in \code{\link[vegan]{plot.cca}}).
#' @param sp_symbol_col character specifying the color for omited species label symbols (\code{+}).
#' @param sp_symbol
#' @param cn_col character specifying the color for centroids (\code{display = "cn"} in \code{\link[vegan]{plot.cca}}).
#' @param ordi_col character specifying the color for \code{\link[vegan]{ordisurf}}.
#' @param envfits_col character vector specifying the colors for \code{\link[vegan]{envfit}} objects.
#' @param base_size base font size for ggplot2 (see \code{\link[ggplot2]{theme_bw}}).
#' @param site_size size parameter for site symbols
#' @param sp_size font size for species labels
#' @param cn_size font size for centroid labels
#' @param ordi_size cex parameter for \code{\link[vegan]{ordisurf}}.
#' @param envfits_size cex parameter for \code{\link[vegan]{envfit}} objects.
#' @details All font sizes should be specified using actual font sizes (not ggplot font sizes).
#' @import ggvegan vegan ggplot2 ggrepel
#' @importFrom Hmisc capitalize
#' @author Mikko Vihtakari with borrowed code from Jari Oksanen
#' @export

# mod = mod1c;
# axes = 1:2; env_data = NULL; axes = 1:2; size_preset = "device"; arrow.mul = NULL; ordisurf_var = NULL; centroids = TRUE; envfits = NULL; sp_repel_method = "orditorp"; sp_label_omit = 0.1; capitalize_cn = TRUE; site_col = "grey70"; sp_col = "firebrick3"; sp_cross_col = "red"; cn_col = "darkolivegreen"; ordi_col = "lightskyblue1"; envfits_col = c("#82C893", "#D696C8", "#056A89", "#B5794C", "#FF9252"); base_size = NULL; site_size = NULL; sp_size = NULL; cn_size = NULL; ordi_size = NULL; envfits_size = NULL; main = NULL

ggord_plot <- function(mod, env_data = NULL, axes = 1:2, size_preset = "device", arrow.mul = NULL, ordisurf_var = NULL, centroids = TRUE, envfits = NULL, sp_repel_method = "orditorp", sp_label_omit = 0.1, capitalize_cn = TRUE, site_col = "grey70", sp_col = "firebrick3", sp_cross_col = "red", cn_col = "darkolivegreen", ordi_col = "lightskyblue1", envfits_col = c("#82C893", "#D696C8", "#056A89", "#B5794C", "#FF9252"), base_size = NULL, site_size = NULL, sp_size = NULL, cn_size = NULL, ordi_size = NULL, envfits_size = NULL, main = NULL) {

## Switches and definitions ####

size.names <- c("base_size", "site_size", "sp_size", "cn_size", "ordi_size", "envfits_size")

sizes <- switch(size_preset,
  pdf = list(base_size = 8, site_size = 0.1, sp_size = 6, cn_size = 6, ordi_size = 0.6, envfits_size = 0.6),
  html = list(base_size = 10, site_size = 1, sp_size = 8, cn_size = 12, ordi_size = 0.6, envfits_size = 0.6),
  device = list(base_size = 10, site_size = 0.1, sp_size = 8, cn_size = 10, ordi_size = 0.6, envfits_size = 0.6),
  stop(paste("size_preset =", size_preset, "not defined"))
)

air <- 1 # orditorp parameter. Add to arguments perhaps

## Data
x <- ggplot2::fortify(mod)

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

    if(nrow(bp) > 0) BPARR <- TRUE else BPARR <- FALSE

  } else {
    CENT <- FALSE
    BPARR <- FALSE
  }
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

axislabs <- axis_expl(mod, axes = axes)

if(is.null(arrow.mul) & CONST) {
  if(BPARR & nrow(bp) > 0) arrow.mul <- max(c(sp$AX1, st$AX1))*0.8/max(bp$AX1) #vegan::ordiArrowMul(mod, display = "sites")
}

# ####

out <- ggplot() +
  geom_hline(yintercept = 0, size = LS(0.5), color = "grey50", linetype = 2) +
  geom_vline(xintercept = 0, size = LS(0.5), color = "grey50", linetype = 2) +
  geom_point(data = st, aes(x = AX1, y = AX2), shape = 20, size = sizes$site_size, color = site_col) +
  xlab(paste0(names(axislabs[1]), " (", sprintf("%.1f", axislabs[1]), "%)")) +
  ylab(paste0(names(axislabs[2]), " (", sprintf("%.1f", axislabs[2]), "%)")) +
  theme_bw(base_size = sizes$base_size) +
  theme(#legend.position = "none",
      axis.line.x = element_blank(),
      axis.ticks = element_line(size = LS(0.5), color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0.5, 0.2, 0.5, 0.1), units = "line"))

if(!is.null(main)) {
  out <- out + ggtitle(main)
}

if(CONST) {
  if(BPARR) {
    out <- out +
    geom_segment(data = bp, aes(x = 0, xend = arrow.mul*AX1, y = 0, yend = arrow.mul*AX2, group = Label),
    arrow = arrow(length = unit(0.5, "lines")), size = LS(1), color = cn_col) +
    geom_text(data = bp, aes(x = arrow.mul*AX1*1.14, y = arrow.mul*AX2*1.04, label = Label, fontface = 2),
    size = FS(sizes$cn_size), color = cn_col)
  }

  if(CENT) {
    out <- out + geom_text(data = cn, aes(x = AX1, y = AX2, label = Label, fontface = 2),
    size = FS(sizes$cn_size), color = cn_col)
  }

}

## Species labels

if(sp_repel_method %in% c("ggrepel", "label_omit")) {

    levels(sp$Label)[levels(sp$Label) %in% sp[abs(sp$AX1) < sp_label_omit & abs(sp$AX2) < sp_label_omit, "Label"]] <- ""
  sp_points <- sp[sp$Label == "",]

} else if(sp_repel_method == "orditorp") {
  ## Code below modified from orditorp. Main author: Jari Oksanen

  labels <- sp$Label
  priority <- rowSums((scale(sp[c("AX1", "AX2")])^2))

  w <- abs(strwidth(labels, cex = 1, units = "figure"))/2 * air
  h <- abs(strheight(labels, cex = 1, units = "figure"))/2 * air

  xx <- cbind(sp[["AX1"]] - w, sp[["AX1"]] + w, sp[["AX2"]] - h, sp[["AX2"]] + h)
  is.na(priority) <- w == 0
  ord <- rev(order(priority, na.last = FALSE))
  xx <- xx[ord, ]
  sp <- sp[ord, ]
  labels <- sp$Label
  tt <- logical(nrow(xx))
  tt[1] <- TRUE
  for (i in 2:(nrow(xx) - sum(is.na(priority)))) {
        j <- 1:(i - 1)
        j <- j[tt[j]]
        tt[i] <- all(xx[i, 1] > xx[j, 2] | xx[j, 1] > xx[i, 2] |
                     xx[i, 3] > xx[j, 4] | xx[j, 3] > xx[i, 4])
    }

  sp_text <- sp[tt, , drop = FALSE]
  sp_points <- sp[!sp$Label %in% unique(sp_text$Label),]

}

if(sp_repel_method == "ggrepel") {
  # ####
  out +
    geom_point(data = sp, aes(x = AX1, y = AX2), shape = "+", color = sp_cross_col, size = FS(sizes$sp_size), alpha = 0.6) +
    geom_text_repel(data = sp, aes(x = AX1, y = AX2, label = Label), color = sp_col, size = FS(sizes$sp_size), fontface = 2, segment.size = LS(0.5), min.segment.length = 0, force = 1)
  # ####
} else if(sp_repel_method == "label_omit") {
  # ####
  out +
  geom_point(data = sp_points, aes(x = AX1, y = AX2), shape = "+", color = sp_cross_col, size = FS(sizes$sp_size)) +
    geom_text(data = sp, aes(x = AX1, y = AX2, label = Label), color = sp_col, size = FS(sizes$sp_size), fontface = 2)
  # ####
} else if(sp_repel_method == "orditorp") {
# ####
  out +
    geom_point(data = sp_points, aes(x = AX1, y = AX2), shape = "+", color = sp_cross_col, size = FS(sizes$sp_size), alpha = 0.5) +
  geom_text(data = sp_text, aes(x = AX1, y = AX2, label = Label), color = sp_col, size = FS(sizes$sp_size), fontface = 2)
# ####
  } else {
  out +
  geom_text(data = sp, aes(x = AX1, y = AX2, label = Label), color = sp_col, size = FS(sizes$sp_size), fontface = 2)
}

}
