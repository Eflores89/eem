#' Interactively order columns in ggplot
#'
#' Order a categorical variable by the values in a numerical variable in ggplot
#' @param data data frame
#' @param axis categorical axis 
#' @param column numerical variable by which to order
#' 
#' @export
#' @examples
#' ggplot(order_axis(df, AXIS_X, COLUMN_Y), aes(x = AXIS_X_o, y = COLUMN_Y))
#' 
order_axis<-function(data, axis, column)
{
  # for interactivity with ggplot2
  arguments <- as.list(match.call())
  col <- eval(arguments$column, data)
  ax <- eval(arguments$axis, data)
  
  # evaluated factors
  a<-reorder(with(data, ax), 
             with(data, col))
  
  #new_data
  df<-cbind.data.frame(data)
  # define new var
  within(df, 
         do.call("<-",list(paste0(as.character(arguments$axis),"_o"), a)))
}
#' Obtain a sample of a dataframe
#'
#' Fast wrapper to extract a sample (with no replacement) of n rows from a data.frame
#' @param data data frame
#' @param n number of rows desired
#' 
#' @export
#' @examples
#' small <- sample_df(largedata, 500)
sample_df<-function(data, n)
{
new <- data[base::sample(nrow(data), n, FALSE), ]
return(new)
}
