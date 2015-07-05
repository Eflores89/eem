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
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' q <- (qplot(carat, price, data=dsamp, colour=clarity)
#'       + ggtitle("Diamonds Are Forever"))
#' q + theme_eem()
#'

theme_eem <- function(font_size = 12, 
                      base_family = "Verdana", 
                      legend_bottom = TRUE,
                      no_legendname = TRUE) 
  {
  theme(
    # --------------------- colores de gráfica en general
    plot.background       = element_rect(colour = "gray", 
                                         fill   =  "#fdf5c9"), #plot area (grande)
    panel.background      = element_rect(fill="#fdf5c9"), #panel (chart area)
    
    # --------------------- titulos
    title                 = element_text(colour = "red", 
                                         size = rel(1.5)),
      plot.title          = element_text(colour = "orange"),
      axis.title.x        = element_text(colour = "blue"),
      axis.title.y        = element_text(),
    
    # --------------------- ejes (menos titulos)
    axis.text             = element_text(colour = "green"),
      axis.text.x         = element_text(angle  = 90),
      axis.text.y         = element_text(colour = "pink"),
    axis.ticks            = element_line(size = 2),
    
    # --------------------- leyenda
    legend.background     = element_rect(colour = "black",
                                         fill = "#ffcb85"),
    legend.position       = if(legend_bottom){"bottom"} else {"right"}, #posicion de leyenda
    legend.title          = if(no_legendname){element_blank()} else {}, #quitar el titulo
    legend.text           = element_text(colour ="white"),
    legend.key            = element_rect(colour = "white", 
                                         fill   = "#ffcb85") #fill de la leyenda
  )
}

#' ggplot color scales for enelmargen.org
#'
#' Style colors used in \emph{www.enelmargen.org} as of july-2015.
#'
#' @return An object of class \code{\link{theme}}.
#'
#' @export
#' @examples
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' q <- (qplot(carat, price, data=dsamp, colour=clarity)
#'       + ggtitle("Diamonds Are Forever"))
#' q + theme_eem()
#'
#'
scales_eem<-function()
{ #traer, del mapeo previo, la información
  
  
  # revisar que sean niveles o continuo
  if(esniveles){
    n<-length(levels(data))
    } else {
      n<-largocomosea
    }
  
  if(n<20)
    {# cuando es 19 o menos, pintamos de manera manual
    scale_colour_manual(values = brewer.pal(n,"Set1")) 
    } else {
    # cuando es mayor a 19, pintamos con gradientes
    scale_color_gradient(low="darkkhaki", high="darkgreen")
    }

}