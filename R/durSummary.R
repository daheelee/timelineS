#' @title Summary for Date Duration Data
#' @description Returns summary statistics for date duration data (for each group if \code{group} is provided)
#' @usage
#' durSummary(df, start, end, group=NA, timeunit="days")
#' @param df Data frame containing start and end dates.
#' @param start Column in df for start dates.
#' @param end Column in df for end dates.
#' @param group Column in df for groups. Default NA.
#' @param timeunit Unit of time to be used in plots. \code{"day(s)","week(s)","month(s)","quarter(s)","semiannual", "halfyear", "half-year", "semi-annual", "year(s)"}
#' @details 1 year = 365.25 days, 1 month = 30.42 days, 1 year = 52.14 weeks
#' @seealso \code{\link{durPlot}}, \code{\link{durCalc}}
#' @examples
#' durSummary(life_exp, start="Birth", end="Death", group="Country", timeunit="years")
#' @author Dahee Lee
#' @import magrittr dplyr
#' @export
#'
durSummary <- function(df, start, end, group=NA, timeunit="days"){

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
