#' ggplot color theme for enelmargen.org
#'
#' Style plots used in \emph{www.enelmargen.org} as of july-2015.
#'
#' \code{theme_eem} implements the standard redish-brown
#' background theme.
#' \code{theme_eem_white} implements a variant with a white
#' panel and background.
#' \code{theme_eem_pdf} implements standard coloring theme optimized for use in rmarkdown to pdf documents.
#'
#' @param font_size font size. Default = 20.
#' @param base_family base family. Default = Verdana.
#' @param legend_bottom will the legend be at the bottom? Default = TRUE
#' @param no_legendname will the legend have no title? Default = TRUE
#'
#' @export
#' @examples
#' library(ggplot2)
#' library(eem)
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' q <- (qplot(carat, price, data=dsamp, colour=clarity)
#'      + labs(title = "Diamonds Are Forever"))
#' q + theme_eem()
#'

theme_eem <- function(font_size = 20, 
                      base_family = "Verdana", 
                      legend_bottom = TRUE,
                      no_legendname = TRUE) 
  {
  theme(
    # --------------------- colores de gráfica en general
    plot.background       = element_rect(colour = "gray", 
                                         fill   =  "#f2f1e8"), #plot area (grande)
    panel.background      = element_rect(fill   = "#F2F0E8"), #panel (chart area)
    panel.grid            = element_line(colour   = "#E5D770"), ##e1b95b,#E5D770
    
    # --------------------- titulos
    title                 = element_text(colour = "black", 
                                         size   = rel(1.5)),
      plot.title          = element_text(colour = "black"),
      axis.title.x        = element_text(colour = "#BDBDBD"),
      axis.title.y        = element_text(colour = "#BDBDBD"),
    
    # --------------------- ejes (menos titulos)
    axis.text             = element_text(colour = "#E5D770"),
      axis.text.x         = element_text(angle  = 90,
                                         colour = "black"),
      axis.text.y         = element_text(colour = "black"),
    axis.ticks            = element_line(size   = 1,
                                        colour  = "white",
                                        linetype = 3),
    #axis.ticks.length     = 10, #requiere unit() con grid
    
    
    # --------------------- leyenda
    legend.background     = element_rect(colour = "white",
                                         fill   = "#F2F0E8"),
    legend.position       = if(legend_bottom){"bottom"} else {"right"}, #posicion de leyenda
    legend.title          = if(no_legendname){element_blank()} else {}, #quitar el titulo
    legend.text           = element_text(colour ="black"),
    legend.key            = element_rect(colour = "white") #fill de la leyenda
  )
}

#' ggplot color scales for enelmargen.org
#'
#' Style colors used in \emph{www.enelmargen.org} as of july-2015.
#'@param levels Number of diferente levels in a factor. If continous = use any number over 20.
#' @export
#' @examples
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' q <- (qplot(carat, price, data=dsamp, colour=clarity)
#'       + ggtitle("Diamonds Are Forever"))
#' q + scale_colour_eem(20)
#'
#'
scale_colour_eem<-function(levels)
{ 
  colors<-c("#A84A44","#E47D04","#D8A19E","#ae8b38","#4d7c28",
            "#38b6a6","#2080c7","#94127a","#155685","#157d85",
            "#731585","#848515","#d06347","#d0ca47","#d04785",
            "#a19c9b","#b5bcbf","#62686b","#021118","#daf3ff")
  if(levels<21){
    use<-colors[1:levels]
    scale_colour_manual(values = use)
  } else {
    scale_color_gradient(low="#ffe7e5", high="#9a2922", na.value = "#bdbdbd")
  }
}

#' ggplot color scales for enelmargen.org
#'
#' Style colors used in \emph{www.enelmargen.org} as of july-2015.
#'
#' @param levels Number of diferente levels in a factor. If continous = use any number over 20.
#' @export
#' @examples
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' q <- (qplot(carat, price, data=dsamp, colour=clarity)
#'       + ggtitle("Diamonds Are Forever"))
#' q + scale_fill_eem(20)
#'
#'
scale_fill_eem<-function(levels)
{ 
  colors<-c("#A84A44","#E47D04","#D8A19E","#ae8b38","#4d7c28",
            "#38b6a6","#2080c7","#94127a","#155685","#157d85",
            "#731585","#848515","#d06347","#d0ca47","#d04785",
            "#a19c9b","#b5bcbf","#62686b","#021118","#daf3ff")
  if(levels<21){
    use<-colors[1:levels]
    scale_fill_manual(values = use)
  } else {
    scale_fill_gradient(low="#ffe7e5", high="#9a2922", na.value = "#bdbdbd")
  }
}
#' ggplot color theme for enelmargen.org
#'
#' Style plots used in \emph{www.enelmargen.org} as of july-2015.
#'
#' \code{theme_eem} implements the standard redish-brown
#' background theme.
#' \code{theme_eem_white} implements a variant with a white
#' panel and background.
#' \code{theme_eem_pdf} implements standard coloring theme optimized for use in rmarkdown to pdf documents.
#'
#' @param font_size font size. Default = 20.
#' @param base_family base family. Default = Verdana.
#' @param legend_bottom will the legend be at the bottom? Default = TRUE
#' @param no_legendname will the legend have no title? Default = TRUE
#'
#' @export
#' @examples
#' library(ggplot2)
#' library(eem)
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' q <- (qplot(carat, price, data=dsamp, colour=clarity)
#'      + labs(title = "Diamonds Are Forever"))
#' q + theme_eem_white()
#'

theme_eem_white <- function(font_size = 20, 
                      base_family = "Verdana", 
                      legend_bottom = TRUE,
                      no_legendname = TRUE) 
{
  theme(
    # --------------------- colores de gráfica en general
    plot.background       = element_rect(colour = "gray", 
                                         fill   =  "white"), #plot area (grande)
    panel.background      = element_rect(fill   = "white"), #panel (chart area)
    panel.grid            = element_line(colour   = "#e1b95b"), #,#E5D770
    
    # --------------------- titulos
    title                 = element_text(colour = "grey50", 
                                         size   = rel(1.5)),
    plot.title          = element_text(colour = "black"),
    axis.title.x        = element_text(colour = "#bf3939"),
    axis.title.y        = element_text(colour = "#bf3939"),
    
    # --------------------- ejes (menos titulos)
    axis.text             = element_text(colour = "#E5D770"),
    axis.text.x           = element_text(angle  = 90,
                                         colour = "black"),
    axis.text.y           = element_text(colour = "black"),
    axis.ticks            = element_line(size   = 1,
                                         colour  = "#F2F0E8",
                                         linetype = 3),
    #axis.ticks.length     = 10, #requiere unit() con grid
    
    
    # --------------------- leyenda
    legend.background     = element_rect(colour = "#F2F0E8",
                                         fill   = "white"),
    legend.position       = if(legend_bottom){"bottom"} else {"right"}, #posicion de leyenda
    legend.title          = if(no_legendname){element_blank()} else {}, #quitar el titulo
    legend.text           = element_text(colour ="black"),
    legend.key            = element_rect(colour = "white") #fill de la leyenda
  )
}

#' ggplot color theme for enelmargen.org
#'
#' Style plots used in \emph{www.enelmargen.org} as of july-2015.
#'
#' \code{theme_eem} implements the standard redish-brown
#' background theme.
#' \code{theme_eem_white} implements a variant with a white
#' panel and background.
#' \code{theme_eem_pdf} implements standard coloring theme optimized for use in rmarkdown to pdf documents.
#'
#'
#' @param font_size font size. Default = 10.
#' @param base_family base family. Default = Verdana.
#' @param legend_bottom will the legend be at the bottom? Default = TRUE
#' @param no_legendname will the legend have no title? Default = TRUE
#'
#' @export
#' @examples
#' library(ggplot2)
#' library(eem)
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' q <- (qplot(carat, price, data=dsamp, colour=clarity)
#'      + labs(title = "Diamonds Are Forever"))
#' q + theme_eem_pdf()
#'
theme_eem_pdf <- function(font_size = 10, 
                          base_family = "Verdana", 
                          legend_right = TRUE,
                          no_legendname = TRUE) 
{
  theme(
    # --------------------- colores de gráfica en general
    plot.background       = element_rect(colour = "white", 
                                         fill   =  "white"), #plot area (grande)
    panel.background      = element_rect(fill   = "white"), #panel (chart area)
    panel.grid            = element_line(colour   = "#E5D770"), ##e1b95b,#E5D770
    
    # --------------------- titulos
    title                 = element_text(colour = "black", 
                                         size   = rel(1)),
    plot.title          = element_text(colour = "black",
                                       size = rel(0.8)),
    axis.title.x        = element_text(colour = "black"),
    axis.title.y        = element_text(colour = "black"),
    
    # --------------------- ejes (menos titulos)
    axis.text             = element_text(colour = "gray"),
    axis.text.x         = element_text(angle  = 90,
                                       colour = "gray"),
    axis.text.y         = element_text(colour = "gray"),
    axis.ticks            = element_line(size   = 1,
                                         colour  = "gray",
                                         linetype = 3),
    # --------------------- leyenda
    legend.background     = element_rect(colour = "white",
                                         fill   = "#F2F0E8"),
    legend.position       = if(legend_right){"right"} else {"bottom"}, #posicion de leyenda
    legend.title          = if(no_legendname){element_blank()} else {}, #quitar el titulo
    legend.text           = element_text(colour ="black"),
    legend.key            = element_rect(colour = "white") #fill de la leyenda
  )
}

#' color list for enelmargen.org graphs
#'
#' Object of type:list, to be used interactively when \code{scale_colour_eem()} fails or is unavailable as a method.
#'
#' @export
#' @examples
#' # see first color:
#' eem_colors[1]
#'
eem_colors <- c("#A84A44", "#E47D04", "#D8A19E", "#ae8b38", "#4d7c28", 
                "#38b6a6", "#2080c7", "#94127a", "#155685", "#157d85", 
                "#731585", "#848515", "#d06347", "#d0ca47", "#d04785", 
                "#a19c9b", "#b5bcbf", "#62686b", "#021118", "#daf3ff")
