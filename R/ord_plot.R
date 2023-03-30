#' @title Alternative ordination plot for CCA and RDA objects (base graphics)
#' @description Alternative to plot vegan's CCA and RDA objects producing illustrative and good looking graphics with less code and need for customization. Plots \code{\link[vegan]{envfit}} and \code{\link[vegan]{ordisurf}} when needed.
#' @param mod \code{\link[vegan]{cca}} or \code{\link[vegan]{rda}} object from vegan.
#' @param env_data optional data frame containing environmental data, which should be fitted using the \code{\link[vegan]{envfit}} and/or \code{\link[vegan]{ordisurf}} functions. The data frame must have the same row order and number than the species data frame used to construct \code{mod}.
#' @param scaling Scaling for species and site scores. See the \code{scaling} argument in \code{\link[vegan]{plot.cca}}).
#' @param size_preset character specifying the size preset for the graphic device. Options: "device" or "pdf". Size of symbols in the plot will be automatically scaled to the device or 122 mm wide pdf (page width for many scientific journals), respectively. You can override the scaling using the \code{*_cex} arguments.
#' @param ordisurf_var character specifying the column name in \code{env_data} that should be used to plot \code{\link[vegan]{ordisurf}}. If \code{NULL}, no \code{\link[vegan]{ordisurf}} will be plotted.
#' @param centroids logical indicating whether CCA or RDA centroids should be plotted (\code{display = "cn"} in \code{\link[vegan]{plot.cca}}).
#' @param envfits character vector specifying column names in \code{env_data}, which should be ploted with the \code{\link[vegan]{envfit}} function.
#' @param orditorp logical indicating whether \code{\link[vegan]{orditorp}} should be used to avoid cluttered species names.
#' @param st_col character specifying the color for sites (\code{display = "sites"} in \code{\link[vegan]{plot.cca}}). Alternatively column name in \code{env_data} and site color will be scaled to levels in the variable. Colors will be taken from the \code{envfits_col} argument.
#' @param sp_col character specifying the color for species (\code{display = "species"} in \code{\link[vegan]{plot.cca}}).
#' @param sp_symbol_col character specifying the color for species symbols.
#' @param sp_arrow logical indicating whether biplot arrows should be plotted for species.
#' @param cn_col character specifying the color for centroids (\code{display = "cn"} in \code{\link[vegan]{plot.cca}}).
#' @param ordi_col character specifying the color for \code{\link[vegan]{ordisurf}}.
#' @param envfits_col character vector specifying the colors for \code{\link[vegan]{envfit}} objects.
#' @param species logical indicating whether to show the species labels.
#' @param main_cex cex parameter for axes titles (\code{\link[graphics]{par}(cex)}).
#' @param sp_font An integer which specifies which font to use for text. If possible, device drivers arrange so that 1 corresponds to plain text (the default), 2 to bold face, 3 to italic and 4 to bold italic. Also, font 5 is expected to be the symbol font, in Adobe symbol encoding. On some devices font families can be selected by family to choose different sets of 5 fonts. From \code{\link[graphics]{par}}.
#' @param st_symbol,sp_symbol \code{\link[graphics]{pch}} code for site and species symbols, respectively.
#' @param latin_sp_names logical indicating whether species abbreviations should be converted to latin names. This functionality is probably not working. Check.
#' @param main character specifying title for the plot. Overlaps with title.main. Fix.
#' @param title.main Overlaps with main. Fix.
#' @param orditorp_air numeric defining the air argument for \code{\link[vegan]{orditorp}}.
#' @param ordihull_st logical indicating whether \code{\link{ordihull}} should be drawn around sites if site colors are scaled to \code{env_data} (see \code{st_col}).
#' @param st_cex cex parameter for site symbols.
#' @param sp_cex,sp_symbol_cex cex parameter for species names and symbols, respectively.
#' @param cn_cex cex parameter for centroids.
#' @param ordi_cex cex parameter for \code{\link[vegan]{ordisurf}}.
#' @param envfits_cex cex parameter for \code{\link[vegan]{envfit}} objects.
#' @param legend_pos character specifying the location of legend (only added if colors are mapped to variables). Set to \code{NULL} to remove the legend.
#' @param axis_labs logical indicating whether to plot axis labels.
#' 
#' @param title.sub character specifying sub-title for the plot.
#' @param ... parameters passed to \code{\link[vegan]{plot.cca}}
#' @details While the function produces OK looking plots at the moment (and they seem to be correct too), the function needs to be rewritten to comply with vegan's "dialect". Also some features are redundant. I will do this rewriting at some point in the future when I have time. Also add examples.
#' @import vegan
#' @importFrom Hmisc capitalize
#' @importFrom graphics points strheight strwidth text arrows
#' @author Mikko Vihtakari with code from Jari Oksanen and Gavin Simpson 
#' @export

# Test params
# env_data = NULL; scaling = "species"; size_preset = "device"; ordisurf_var = NULL; centroids = TRUE; envfits = NULL; orditorp = TRUE; orditorp_air = 1; st_col = "grey70"; st_symbol = "+"; ordihull_st = FALSE; species = TRUE; sp_col = "black"; sp_arrow = FALSE; sp_symbol = 20; sp_symbol_col = "black"; sp_font = 3; latin_sp_names = FALSE; cn_col = "darkolivegreen"; ordi_col = c("lightskyblue1", "lightsalmon1"); envfits_col = c("#649971", "#A47399", "#03617E", "#B5794C", "#FF9252", "#FF5F68", "#449BCF"); main_cex = NULL; st_cex = NULL; sp_cex = NULL; sp_symbol_cex = NULL; cn_cex = NULL; ordi_cex = NULL; envfits_cex = NULL; main = NULL; axis_labs = TRUE; legend_pos = "topleft"; title.main = NULL; title.sub = NULL

ord_plot <- function(mod, env_data = NULL, scaling = "species", size_preset = "device", ordisurf_var = NULL, centroids = TRUE, envfits = NULL, orditorp = TRUE, orditorp_air = 1, st_col = "grey70", st_symbol = "+", ordihull_st = FALSE, species = TRUE, sp_col = "black", sp_arrow = FALSE, sp_symbol = 20, sp_symbol_col = "black", sp_font = 3,  latin_sp_names = FALSE, cn_col = "darkolivegreen", ordi_col = c("lightskyblue1", "lightsalmon1"), envfits_col = c("#649971", "#A47399", "#03617E", "#B5794C", "#FF9252", "#FF5F68", "#449BCF"), main_cex = NULL, st_cex = NULL, sp_cex = NULL, sp_symbol_cex = NULL, cn_cex = NULL, ordi_cex = NULL, envfits_cex = NULL, main = NULL, axis_labs = TRUE, legend_pos = "topleft", title.main = NULL, title.sub = NULL, ...) {
  
  size.names <- c("main_cex", "st_cex", "sp_cex", "sp_symbol_cex", "cn_cex", "ordi_cex", "envfits_cex")
  
  sizes <- switch(size_preset,
                  pdf = list(main_cex = 0.8, st_cex = 0.7, sp_cex = 0.7, sp_symbol_cex = 0.3, cn_cex = 0.8, ordi_cex = 0.5, envfits_cex = 0.8),
                  device = list(main_cex = 0.9, st_cex = 0.7, sp_cex = 0.8, sp_symbol_cex = 0.3, cn_cex = 0.9, ordi_cex = 0.7, envfits_cex = 1),
                  stop(paste("size_preset =", size_preset, "not defined"))
  )
  
  # k <- size.names[4]
  sizes <- lapply(size.names, function(k) {
    if(is.null(get(k))) {
      sizes[[k]]
    } else {
      if(!is.numeric(get(k))) stop(paste("size parameter", k, "has to be numeric"))
      sizes[[k]] <- get(k)
    }
  })
  
  names(sizes) <- size.names
  
  graphics::par(cex = sizes$main_cex)
  
  ## Base plot
  
  plot(mod, xlab = "", ylab = "", type = "n", main = main, scaling = scaling, ...)
  
  if(axis_labs) graphics::title(xlab = paste0(names(axis_expl(mod)[1]), " (", sprintf("%.1f", axis_expl(mod)[1]), "%)"), ylab = paste0(names(axis_expl(mod)[2]), " (", sprintf("%.1f", axis_expl(mod)[2]), "%)"))
  
  ## Ordisurf
  
  if(!is.null(ordisurf_var)) {
    
    if(length(ordisurf_var) > 1) {
      SURFMAP <- TRUE
    } else {
      SURFMAP <- FALSE
    }
    
    for(i in 1:length(ordisurf_var)) {
      vegan::ordisurf(mod, env_data[[ordisurf_var[i]]], add = TRUE, col = ordi_col[i], labcex = sizes$ordi_cex, lwd.cl = sizes$ordi_cex+0.2, scaling = scaling)
    }
  } else {
    SURFMAP <- FALSE
  }
  
  ## Sites
  
  if(!is.null(env_data) & st_col %in% names(env_data)) {
    levs <- unique(env_data[[st_col]])
    STMAP <- TRUE
    for(i in seq_along(levs)) {
      points(mod, display = "sites", pch = st_symbol, cex = sizes$st_cex, col = envfits_col[i], scaling = scaling, select = env_data[[st_col]] == levs[i])
    }
  } else {
    STMAP <- FALSE
    points(mod, display = "sites", pch = st_symbol, cex = sizes$st_cex, col = st_col, scaling = scaling)
  }
  
  ## Ordihull
  
  if(ordihull_st) {
    vegan::ordihull(mod, env_data[[st_col]], col = envfits_col[1:length(levs)], alpha = 0.5, cex = sizes$ordi_cex)
  }
  
  ## Species
  
  if(sp_arrow) {
    g <- scores(mod, display = "sp", scaling = scaling)
    
    if (!species) {
      arrlen <- 1
    } else {
      arrlen <- 0.85
    }
    
    arrows(0, 0, g[,1] * arrlen, g[, 2] * arrlen,
           col = sp_col, cex = sizes$sp_cex, length = 0.05)
  }
  
  if(species) {
    
    sp_nams <- data.frame(scores(mod, scaling = 0, display = "sp"))
    sp_nams$priority <- unname(rowSums(abs(sp_nams[1:2])))
    sp_nams$label <- rownames(sp_nams)
    
    if(latin_sp_names) {
      sp_nams$label <- gsub(" ", "\n", sp_nams$label)
      #sp_nams$label <- italicize_taxa(sp_nams$label, type = "ggplot")
    }
    
    if(orditorp) {
      vegan::orditorp(mod, display = "sp", pcol = sp_symbol_col, pch = sp_symbol, air = orditorp_air, col = sp_col, cex = sizes$sp_cex, pcex = sizes$sp_symbol_cex, font = sp_font, scaling = scaling, labels = sp_nams$label, priority = sp_nams$priority)
    } else {
      text(mod, display = "sp", col = sp_col, cex = sizes$sp_cex, font = sp_font, scaling = scaling)
    }
  }
  
  ## Envfit
  
  if(!is.null(env_data) & is.null(envfits) & is.null(mod$CCA)) {
    envfits <- apply(env_data, 2, function(k) length(unique(k)) > 1)
    envfits <- names(envfits)[envfits]
  }
  
  if(!is.null(envfits) & is.vector(envfits)) {
    if(length(envfits) == 1 & all(envfits == st_col) & STMAP) {
      ef <- vegan::envfit(mod, env_data[envfits])
      
      labs <- list(vectors = row.names(ef$vectors$arrows), factors = gsub(paste(names(env_data), collapse = "|"), "", rownames(ef$factors$centroids)))
      
      plot(ef, add = TRUE, col = envfits_col[1:length(levs)], label = labs, cex = sizes$envfits_cex, font = 2)
      
    } else {
      
      for(k in envfits) {
        ef <- vegan::envfit(mod, env_data[k])
        
        labs <- list(vectors = row.names(ef$vectors$arrows), factors = gsub(paste(names(env_data), collapse = "|"), "", rownames(ef$factors$centroids)))
        
        plot(ef, add = TRUE, col = envfits_col[which(envfits == k)], label = labs, cex = sizes$envfits_cex, font = 2)
      }
    }
  } 
  
  ## Centroids and biplot arrows
  
  if(!is.null(mod$CCA) & centroids) {
    
    consts <- sapply(env_data[names(mod$terminfo$ordered)], class)
    cents <- names(consts[consts %in% c("factor", "character")])
    
    if(is.null(mod$CCA$centroids)) {
      text(mod, display = "bp", col = cn_col, cex = sizes$cn_cex, font = 2, scaling = scaling)
    } else {
      # labs <- Hmisc::capitalize(gsub(paste(cents, collapse = "|"), "", rownames(mod$CCA$centroids)))
      text(mod, display = "cn", 
           # labels = labs, 
           col = cn_col, cex = sizes$cn_cex, font = 2, scaling = scaling)
    }
  }
  
  ## Legends
  
  if((SURFMAP | STMAP) & !is.null(legend_pos)) {
    if(STMAP) {
      graphics::legend(x = legend_pos, legend = levs, col = envfits_col[1:length(levs)], pch = st_symbol, bty = "n", title = Hmisc::capitalize(gsub("\\.", " ", st_col)))
    }
    if(SURFMAP) {
      graphics::legend(x = legend_pos, legend = ordisurf_var, col = ordi_col[1:length(ordisurf_var)], lty = 1, bty = "n", title = "Ordisurf variables", yjust = 0)
    }
  }
  
  ## Title
  
  if(!is.null(title.main) | !is.null(title.sub)) {
    
    graphics::title(main = title.main, sub = title.sub)  
  }
  
}