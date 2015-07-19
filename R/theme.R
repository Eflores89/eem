#' ggplot color theme for enelmargen.org
#'
#' Style plots used in \emph{www.enelmargen.org} as of july-2015.
#'
#' \code{theme_eem} implements the standard redish
#' background theme.
#' \code{theme_eem_white} implements a variant with a white
#' panel and background.
#'
#' \emph{eem} uses "ITC Officina Sans" as its font for graphs. If
#' you have access to this font, you can use it with the
#' \pkg{extrafont} package. "Verdana" is a good substitute.
#'
#' @param font_size mmmm
#' @param base_family mmm
#' @param legend_bottom aaa
#' @param no_legendname aaa
#'
#' @return An object of class \code{\link{theme}}.
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
#'
#' @return An object of class \code{\link{theme}}.
#' @note not ready
#' @export
#' @examples
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' q <- (qplot(carat, price, data=dsamp, colour=clarity)
#'       + ggtitle("Diamonds Are Forever"))
#' q + scales_eem()
#'
#'
scales_eem<-function(esniveles)
{ ### not run 
  if(esniveles){
    n<-length(levels(data))
    } else {
      n<-largocomosea
    }
  
  if(n<20)
    {# cuando es 19 o menos, pintamos de manera manual
    scale_colour_manual(values = c("#A84A44","#E47D04","#D8A19E","#ae8b38",
                                   "#4d7c28","#38b6a6","#2080c7","#ce726e")) 
    } else {
    # cuando es mayor a 19, pintamos con gradientes
    scale_color_gradient(low="darkkhaki", high="darkgreen")
    }

}
