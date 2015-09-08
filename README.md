# eem (En El Margen)
### Themes & shortcut functions for writing.
These are collections of graph themes for www.enelmargen.org and shortcut functions for data analysis for personal projects.
___________________
### Themes & Colors
#### Themes
- `theme_eem()` Produces a consistent theme for ggplot2 graphs (used in www.enelmargen.org since july-2015).
- `theme_eem_white()` Produces a consistent theme for ggplot2 graphs, similar to `theme_eem()`, but with a white background. 

#### Colors
`scale_colour_eem()` and `scale_fill_eem()` pass manual values to scale colors for ggplot2 objects. This allows consistency with the theme described earlier. The argument takes a value between 1 and 20 for discrete values (if there are less than 20 factors). Any number higher than 20 will use a gradient function. 

#### Usage
```
#install first!
library(devtools)
install_github("eflores89/eem")

library(ggplot2)
library(eem)
# sample data
dsamp <- diamonds[sample(nrow(diamonds), 1000), ] 

q <- (qplot(carat, price, data=dsamp, colour=clarity) 
      + labs(title = "Diamonds Are Forever")) 

# add theme
p<- q + theme_eem() 
# add theme and colors
p<- q + theme_eem() + scale_colour_eem(20)

plot(p)
```

