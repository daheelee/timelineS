#' @title Faceted Timelines for Grouped Data
#' @description Plots faceted timelines for grouped data.
#' @usage
#' timelineG(df, start, end, names, phase = NA, group1 = NA, group2 = NA,
#'           width = 2, color = "grey", theme = NULL, other = NULL)
#' @param df Data frame containing start dates, end dates, groups, phases, and names for each timeline.
#' @param start Column in df for start dates.
#' @param end Column in df for end dates.
#' @param names Column in df for names of each timeline
#' @param phase Column in df for phases.
#' @param group1 Column in df for groups to be used as the rows of the tabular display. Default is NA.
#' @param group2 Column in df for groups to be used as the columns of the tabular display. Default is NA.
#' @param width Width of each timeline. Default is 2.
#' @param color Color of timelines, only used when \code{phase} is not provided.
#' @param theme Add theme elements if needed.
#' @param other Add other elements if needed.
#' @seealso \code{\link{timelineS}}
#' @examples
#' ### Plot timelines row-grouped by "Country"
#' timelineG(df = life_country, start = "Start", end = "End", names = "Name",
#' phase = "Phase", group1 = "Country")
#'
#' ### Plot timelines row-grouped by "Country" and column-grouped by "Gender"
#' timelineG(df = life_country, start = "Start", end = "End", names = "Name",
#' phase = "Phase", group1 = "Country", group2 = "Gender")
#'
#'  ### Plot timelines, no group
#' timelineG(df = life_country, start = "Start", end = "End", names = "Name",color = "grey")
#'
#' @author Dahee Lee
#' @import ggplot2 graphics
#' @export
#'

timelineG <- function(df, start, end, names, phase = NA, group1 = NA, group2 = NA,
                      width = 2, color = "grey", theme = NULL, other = NULL){

  if(!is.data.frame(df)){
    stop("'df' must be a data frame")
  }

  if(!(class(df[[start]]) == "Date") && (class(df[[end]]) == "Date")){
    stop("'start' and 'end' must be dates")
  }

  facets <- if (is.na(group1) & is.na(group2)){
    paste(names, "~.")
  } else if (is.na(group2)){
    paste(group1, "~.")
  } else {
    paste(group1, "~", group2)
  }

  a <- if(is.na(phase)){
    ggplot(df, aes_string(x = start, y = names)) +
      geom_segment(aes_string(x = start, xend = end, y = names, yend = names), size = width, color = color)
  } else {
    ggplot(df, aes_string(x = start, y = names, color = phase)) +
      geom_segment(aes_string(x = start, xend = end, y = names, yend = names), size = width)
  }

  a <- if (is.na(group1) & is.na(group2)){
    a
  } else {
    a + facet_grid(facets, scales = "free_y", space = "free_y", drop = TRUE)
  }

  a <- a + theme + other
  plot(a)
}
