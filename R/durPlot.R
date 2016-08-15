#' @title Graphs and Summary for Date Durations
#' @description Plots boxplot, histogram, density plot, scatter plot, line plot and prints summary statistics for date duration data.
#' @usage
#' durPlot(df, start, end, group=NA, timeunit="days", plot_type="all",
#'        facet=FALSE, facet.nrow=NULL, theme=NULL, other=NULL,
#'        fill_color="black", line_color="black", groupcolor=TRUE,
#'        point_size=2, alpha=NA, binwidth=0.5, show_legend=TRUE,
#'        title=FALSE, title_boxplot="Boxplot", title_histogram="Histogram",
#'        title_density="Density Plot", title_scatter="Scatter Plot", title_line="Line Plot")
#' @param df Data frame containing start dates, end dates and groups.
#' @param start Column in df for start dates.
#' @param end Column in df for end dates.
#' @param group Column in df for groups. Default is NA.
#' @param timeunit Unit of time to be used in plots.
#' \code{"day(s)","week(s)","month(s)","quarter(s)","semiannual", "halfyear", "half-year", "semi-annual", "year(s)"}. Default is \code{"days"}.
#' @param plot_type One of \code{"all", "boxplot", "histogram", "density", "scatter", "line"}. Default is \code{"all"}.
#' @param facet If TRUE, wraps plots in group facets
#' @param facet.nrow Number of rows for facet wrap
#' @param theme Add theme elements if needed.
#' @param other Add other elements if needed.
#' @param fill_color Fill color
#' @param line_color Line color
#' @param groupcolor If FALSE, fill_color and line_color used for all groups. Default is TRUE.
#' @param point_size Point size for scatterplot
#' @param alpha Color transparency [0,1]
#' @param binwidth Binwidth for histogram; default 0.5.
#' @param show_legend Default is TRUE
#' @param title If TRUE, puts main titles for each plot
#' @param title_boxplot Title for boxplot title
#' @param title_histogram Title for histogram
#' @param title_density Title for density plot
#' @param title_scatter Title for scatter plot
#' @param title_line Title for line plot
#' @details The function also returns summary statistics for the specified date duration.
#' @seealso \code{\link{timelineS}}, \code{\link{timelineG}}, \code{\link{durSummary}}, \code{\link{durCalc}}
#' @examples
#' durPlot(life_exp, start="Birth", end="Death", group="Country",
#' timeunit="years", facet=TRUE, binwidth=3, alpha=0.7, title=TRUE)
#' durPlot(life_exp, start="Birth", end="Death", group="Country",
#' timeunit="years",alpha=0.5, title=TRUE)
#' @author Dahee Lee
#' @import ggplot2 magrittr dplyr
#' @export

durPlot <- function(df, start, end, group=NA, timeunit="days", plot_type="all",
                    facet=FALSE, facet.nrow=NULL, theme=NULL, other=NULL,
                    fill_color="black", line_color="black", groupcolor=TRUE,
                    point_size=2, alpha=NA, binwidth=0.5, show_legend=TRUE,
                    title=FALSE, title_boxplot="Boxplot", title_histogram="Histogram",
                    title_density="Density Plot", title_scatter="Scatter Plot", title_line="Line Plot"){

  if(!is.data.frame(df)){
    stop("'df' must be a data frame")
  }

  if(!(class(df[[start]])=="Date")&&(class(df[[end]])=="Date")){
    stop("'start' and 'end' must be dates")
  }

  # Remove NA rows
  df <- df[rowSums(is.na(df[,c(start,end)]))==0,]

  # Make duration data
  df$duration <- as.numeric(df[[end]]-df[[start]])

  # Change Time unit
  df$duration <- if(timeunit %in% c("year","years")){
    df$duration/365.25
  } else if (timeunit %in% c("semiannual", "halfyear", "half-year", "semi-annual")){
    df$duration/365.25*2
  } else if (timeunit %in% c("quarter", "quarters")){
    df$duration/365.25*4
  } else if (timeunit %in% c("month", "months")){
    df$duration/30.42
  } else if (timeunit %in% c("week", "weeks")){
    df$duration/52.14
  } else if (timeunit %in% c("day", "days")){
    df$duration
  } else {
    warning("invalid 'timeunit': default 'days' used")
    df$duration }

  # No Group
  if (is.na(group)){

    b <-ggplot(df, aes_string(x=factor(""), y="duration")) + geom_boxplot(alpha=alpha, fill=fill_color, color=line_color) + xlab("")
    h <-ggplot(df, aes_string(x="duration")) + geom_histogram(alpha=alpha, fill=fill_color, color=line_color, binwidth=binwidth)
    d <-ggplot(df, aes_string(x="duration")) + geom_density(alpha=alpha, fill=fill_color, color=line_color)
    s <-ggplot(df, aes_string(x=start, y="duration")) + geom_point(alpha=alpha, fill=fill_color, color=line_color, size=point_size)
    l <-ggplot(df, aes_string(x=start, y="duration")) + geom_line(alpha=alpha, color=line_color)

    # Group
  } else {

    b <- ggplot(df, aes_string(x=group, y="duration"))
    h <- ggplot(df, aes_string(x="duration"))
    d <- ggplot(df, aes_string(x="duration"))
    s <- ggplot(df, aes_string(x=start, y="duration", group=group))
    sf <- ggplot(df, aes_string(x=start, y="duration")) + geom_point(data = df[, !(colnames(df) %in% group)], color="grey85", size=point_size)
    l <- ggplot(df, aes_string(x=start, y="duration"))

    # Group color
    if(groupcolor){

      b <- b + geom_boxplot(aes_string(fill=group, color=group), alpha=alpha, show.legend=show_legend, outlier.size=point_size)
      h <- h + geom_histogram(aes_string(fill=group, color=group), alpha=alpha, show.legend=show_legend, binwidth=binwidth)
      d <- d + geom_density(aes_string(fill=group, color=group), alpha=alpha, show.legend=show_legend)
      s <- s + geom_point(aes_string(fill=group, color=group), alpha=alpha, show.legend=show_legend, size=point_size)
      sf<- sf + geom_point(aes_string(fill=group, color=group), alpha=alpha, show.legend=show_legend, size=point_size)
      l <- l + geom_line(aes_string(color=group), alpha=alpha, show.legend=show_legend)

    } else {

      b <- b + geom_boxplot(fill=fill_color, color=line_color, alpha=alpha, show.legend=show_legend, outlier.size=point_size)
      h <- h + geom_histogram(fill=fill_color, color=line_color, alpha=alpha, show.legend=show_legend, binwidth=binwidth)
      d <- d + geom_density(fill=fill_color, color=line_color, alpha=alpha, show.legend=show_legend)
      s <- s + geom_point(fill=fill_color, color=line_color, alpha=alpha, show.legend=show_legend, size=point_size)
      sf<- sf + geom_point(fill=fill_color, color=line_color, alpha=alpha, show.legend=show_legend, size=point_size)
      l <- l + geom_line(color=line_color, alpha=alpha, show.legend=show_legend)
    }
  }

  # Facet
  if(is.na(group)){
    facet=FALSE
  }

  if(facet){
    b <- b
    h <- h + facet_wrap(stats::as.formula(paste("~",group)), nrow=facet.nrow)
    d <- d + facet_wrap(stats::as.formula(paste("~",group)), nrow=facet.nrow)
    s <- sf + facet_wrap(stats::as.formula(paste("~",group)), nrow=facet.nrow)
    l <- l + facet_wrap(stats::as.formula(paste("~",group)), nrow=facet.nrow)
  }

  # Plot Titles
  if(title){
    b <- b + ggtitle(title_boxplot)
    h <- h + ggtitle(title_histogram)
    d <- d + ggtitle(title_density)
    s <- s + ggtitle(title_scatter)
    l <- l + ggtitle(title_line)
  }

  # Add theme and other
  b <- b + theme + other
  h <- h + theme + other
  d <- d + theme + other
  s <- s + theme + other
  l <- l + theme + other

  if (plot_type=="boxplot"){
    print(b)
  } else if (plot_type=="histogram"){
    print(h)
  } else if (plot_type=="density"){
    print(d)
  } else if (plot_type=="scatter"){
    print(s)
  } else if (plot_type=="line"){
    print(l)
  } else if (plot_type=="all"){
    print(b)
    print(h)
    print(d)
    print(s)
    print(l)
  }

  # Print Summary
  df <- if (is.na(group)){df} else (df %>% group_by_(as.name(group)))
  summary <- df %>%
    summarize_(min = ~min(duration, na.rm = T),
               Qt1 = ~quantile(duration, probs = 0.25, na.rm = T),
               median = ~median(duration, na.rm = T),
               mean = ~mean(duration, na.rm = T),
               Qt3 = ~quantile(duration, probs = 0.75, na.rm = T),
               max = ~max(duration, na.rm = T),
               sd = ~sd(duration, na.rm = T))
  if(is.na(group)){
    summary <- round(summary[,(ncol(summary)-6):ncol(summary)],2)
  } else {
    summary <- cbind(summary[,1:(ncol(summary)-7)],round(summary[,(ncol(summary)-6):ncol(summary)],2))
  }
  return(summary)
}


