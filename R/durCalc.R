#' @title Filter Dataset by Date Duration
#' @description Calculates the duration between two dates, use it as a filter to select rows that satisfy the length criteria.
#' Returns the dataset with additional columns regarding the length of durations in different units.
#' @usage
#' durCalc(df=NULL, start, end, timeunit = "day", filterlength=NA, filterlonger=TRUE,
#'        year=365.25, month=30.42)
#' @param df Data frame containing start and end dates.
#' @param start Column in df for start dates or a date to use as start date.
#' @param end Column in df for end dates or a date to use as a end date.
#' @param timeunit Unit of time to be used in plots. \code{"day(s)","week(s)","month(s)","quarter(s)","semiannual", "halfyear", "half-year", "semi-annual", "year(s)"}
#' @param filterlength A time length to use as filter.
#' @param filterlonger If TRUE, the function gives rows with longer durations than specified in filterlength. If FALSE, gives rows with shorter durations.
#' @param year Number of days to use as a year. Default is 365.25.
#' @param month Number of days to use as a month. Default is 30.42.
#' @details
#' Additional columns returned with the filtered rows are: length of duration in days, in specified time unit, and in calendar units, and
#' how much longer/shorter the durations are compared to filter length in calendar units.
#'
#' If no \code{filterlength} is provided, then returns all rows with length of duration in days and calendar units.
#'
#' You can use dates for \code{start} and \code{end} and provide no \code{df} to get the length of duration between the dates in calendar units. See example.
#' @return A subset of original data frame with additional columns in specified time units and calendar units.
#' @seealso \code{\link{durPlot}}, \code{\link{durSummary}}
#' @examples
#' durCalc(life_exp, start="Birth", end="Death", timeunit="years", filterlength=85)
#' durCalc(life_exp, start="Birth", end=as.Date("2000-1-1"), timeunit="years")
#' durCalc(start=as.Date("2010-12-1"), end=as.Date("2015-4-26"), timeunit="weeks")
#' @author Dahee Lee
#' @export

durCalc <- function(df=NULL, start, end, timeunit = "day",
                    filterlength=NA, filterlonger=TRUE, year=365.25, month=30.42){

  if(is.null(start)){stop("'start' needed")}
  if(is.null(end)){stop("'end' needed")}

  # Make duration data for calculation
  if (!is.null(df)){
    if (class(end)=="Date" & class(start)=="character"){
      df$days <- as.numeric(end-df[[start]])
    } else if (class(start)=="Date" & class(end)=="character"){
      df$days <- as.numeric(df[[end]]-start)
    } else if (class(start)=="character" & class(end)=="character") {
      df$days <- as.numeric(df[[end]]-df[[start]])
    } else {
      stop("'start and 'end' have to be dates or column names in 'df'")
    }

    # Remove missing rows
    df <- df[!is.na(df$days),]

  } else {

    if (class(start)=="Date" & class(end)=="Date"){
      days <- as.numeric(end-start)
    } else {
      stop("'start and 'end' have to be dates")
    }

  }

  # Change Time unit
  unit <- if(timeunit %in% c("year","years")){
    1/year
  } else if (timeunit %in% c("semiannual", "halfyear", "half-year", "semi-annual")){
    2/year
  } else if (timeunit %in% c("quarter", "quarters")){
    4/year
  } else if (timeunit %in% c("month", "months")){
    1/month
  } else if (timeunit %in% c("week", "weeks")){
    1/7
  } else if (timeunit %in% c("day", "days")){
    1
  } else {
    warning("invalid 'timeunit': default 'days' used")
    1 }

  # Make diff_timeunit column
  if (!is.null(df)){
    if(!is.na(filterlength)){
      df$diff_timeunit <- df$days*unit
    }
  } else {
    diff_timeunit <- days*unit
  }

  # Duration in calendar terms
  calc_diffdays <- function(days){
    # Year
    Yr <- floor((days)/year)
    outputYr <- paste0(Yr,ifelse(Yr>1, "years ", "year "))
    remaining_days <- (days)%%year
    # Month
    M <- floor(remaining_days/month)
    outputM <- paste0(M,ifelse(M>1, "months ", "month "))
    # Day
    D <- remaining_days %% month
    outputStr <- paste0(if(Yr>=1){outputYr}, if(M>=1){outputM}, round(D,0), ifelse(D>1,"days","day"))
    return(outputStr)
  }

  # Select
  if(!is.null(df)){

    if (is.na(filterlength)){
      df$diff_length <- sapply(df$days, calc_diffdays)

    } else {

      if (filterlonger==TRUE){
        df <- df[df$diff_timeunit > filterlength, ]
        df$diff_timeunit <- round(df$days*unit,2)
        df$late_days <- df$days - filterlength/unit
        df$diff_length <- sapply(df$days, calc_diffdays)
        df$longer_by <- sapply(df$late_days, calc_diffdays)
        names(df)[names(df) == "diff_timeunit"] <- paste0("diff_", timeunit)
        df$late_days <- NULL

      } else {

        df <- df[df$diff_timeunit < filterlength, ]
        df$diff_timeunit <- round(df$days*unit,2)
        df$early_days <- filterlength/unit - df$days
        df$diff_length <- sapply(df$days, calc_diffdays)
        df$shorter_by <- sapply(df$early_days,calc_diffdays)
        names(df)[names(df) == "diff_timeunit"] <- paste0("diff_", timeunit)
        df$early_days <- NULL
      }
    }

  } else {

    diff_timeunit <- round(days*unit,2)
    diff_length <- calc_diffdays(days)
    df <- data.frame(days, diff_timeunit, diff_length)
    names(df)[names(df) == "diff_timeunit"] <- paste0("diff_",timeunit)
  }

  # Print result
  if (nrow(df)==0){
    print("No data meeting criteria")
  } else {return(df)}
}
