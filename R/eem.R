#' Interactively order columns in ggplot
#'
#' Order a categorical variable by the values in a numerical variable in ggplot
#' @param data data frame
#' @param axis categorical axis 
#' @param column numerical variable by which to order
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @export
#' @examples
#' ggplot(order_axis(df, AXIS_X, COLUMN_Y), 
#'        aes(x = AXIS_X_o, y = COLUMN_Y))
#' 
order_axis<-function(data, axis, column)
{
  # for interactivity with ggplot2
  arguments <- as.list(match.call())
  
  #if tidy information with more than 1 column 
  #if(length(names(data))>2)
  #{
   # data <- data %>% 
    #  dplyr::group_by(eval(arguments$axis)) %>% 
     # dplyr::summarise(newsum = sum(column))
  #  data <- as.data.frame(data) 
   # names(data)<-c(as.character(arguments$axis), as.character(arguments$column))
  #} else {}

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
#' @param bagging with replacement? TRUE for bagging
#' 
#' @export
#' @examples
#' small <- sample_df(largedata, 500)
sample_df<-function(data, n, bagging = FALSE)
{
new <- data[base::sample(nrow(data), n, replace = bagging), ]
return(new)
}

#' Split a df into random sets
#'
#' Fast wrapper to split a data frame into random training and test sets
#' @param data data frame
#' @param p percent desired in training set (test will be 1-p)
#' @note returns list with two data frames
#' @export
#' @examples
#' sets <- split_df(largedata, 0.75)

split_df <- function(data, p) {
training <- eem::sample_df(as.data.frame(data),
                  round(length(as.data.frame(data)[,1])*p,0))
test <- subset(as.data.frame(data), 
                     !(row.names(data) %in% row.names(training)))
l <- list(training=training,
          test=test)
return(l)
}
#' Anonimize a column in a data frame
#'
#' Fast wrapper make a column in a data frame anonymous
#' @param data data frame
#' @param column number of column to make anonymous
#' @param catalog if TRUE returns a catalog to bind the anonymous to the original data
#' @examples
#' df_anon <- anonymize(largedata, 1)
#' @export

anonymize <- function (data, column, catalog = FALSE) 
{ vector <- data[,column]

    a <- length(vector)
    b <- length(unique(vector))
    if (b > 1000000) {
        stop("Too many unique values. Try diferent method")
    }
    else {
    }
    to_anon <- unique(vector)
    new_value <- sample(x = 1:(1000000-1), 
                        size = b, 
                        replace = FALSE)
    base_cat <- cbind.data.frame(to_anon, new_value)
    vector_new <- base_cat[match(x = vector, table = base_cat$to_anon), ]
    out <- as.vector(vector_new$new_value)
    data[,column]<-out
    out_df <- data
    
    if (catalog) {
        l <- list(base_cat, out_df)
        return(l)
    }
    else {
        return(out_df)
    }
}
#' Load an RData file interactively
#' 
#' Nifty wrapper function to load into a named object, an RData file. Taken from \url{http://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file}
#' @params filename R object to be loaded
#' @examples 
#' #Notrun
#' df <- loadRData("Data/data.RData")
#' @export
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
#' Create a Calendar with ggplot2
#'
#' Create a calendar with ggplot2 out of a data.frame with "dates" and "counts" columns. Uses ggplot2 and lubridate.
#' @param df data.frame with "dates" and "counts" columns.
#' @param color_fill The color to fill date with
#' @param only_count Show no legend (we only have pairs of 1 or 0/NA)
#' @param title The title of the calendar
#' @export
#' @examples
#' gg_cal(df)
#'
gg_cal <- function(df, color_fill = "grey90", only_count = TRUE, title = "Calendar") {
    require(ggplot2)
    require(lubridate)
    wom <- function(date) { # week-of-month
      first <- wday(as.Date(paste(year(date),month(date),1,sep="-")))
      return((mday(date)+(first-2)) %/% 7+1)
    }
    
    df$month <- month(df$dates)
    df$day   <- mday(df$dates)
    
    rng   <- range(df$dates)
    rng   <- as.Date(paste(year(rng),month(rng),1,sep="-"))
    start <- rng[1]
    end   <- rng[2]
    month(end) <- month(end)+1
    day(end)   <- day(end)  -1
    
    cal <- data.frame(dates=seq(start,end,by="day"))
    cal$year  <- year(cal$date)
    cal$month <- month(cal$date)
    cal$cmonth<- month(cal$date,label=T)
    cal$day   <- mday(cal$date)
    cal$cdow  <- wday(cal$date,label=T)
    cal$dow   <- wday(cal$date)
    cal$week  <- wom(cal$date)
    
    cal        <- merge(cal,df[,c("dates","counts")],all.x=T)
    
    p <- ggplot(cal, 
           aes(x = cdow, y = -week))+
      geom_tile(
        aes(fill = counts, colour = color_fill))+
      geom_text(
        aes(label=day),
        size=3,
        colour="grey20")+
      facet_wrap( ~cmonth, ncol=3)+
      scale_fill_gradient(
        low = "moccasin", high = "dodgerblue", na.value="white")+
      scale_color_manual(
        guide = F, values = "grey50")+
      scale_x_discrete(
        labels = c("S","M","T","W","Th","F","S"))+
      theme(
        axis.text.y = element_blank(), axis.ticks.y = element_blank())+
      theme(panel.grid = element_blank())+
      labs(x="", y="", title = title)+
      coord_fixed()
    
    if(only_count){
      p <- p + theme(legend.position = "none")
    }
    
    print(p)
}
#' Create a data.frame with dates in a year, to be used by gg_cal
#'
#' Create dates that span a year
#' @param year a year in numeric (i.e. 2016)
#' @export
#' @examples
#' # to make a year data.frame with some important dates
#' library(dplyr)
#' make_year(2016) %>% left_join(., data.frame("dates" = c("2016-09-09", "2016-08-28"), counts = c(1,1)))
#' 
#'
make_year <- function(year){
  start <- as.Date(paste0("01/01/",year), format = "%m/%d/%Y")
  end <- as.Date(paste0("12/31/",year), format = "%m/%d/%Y")
  require(lubridate)
  d <- data.frame("dates" = seq(start, end, by="day"))
  return(d)
}
