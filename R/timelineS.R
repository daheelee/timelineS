#' @title Timeline with Event Labels
#' @description Plots a horizontal timeline with event descriptions at corresponding dates.
#' @usage
#' timelineS(df, main = NA, xlab = NA, buffer.days = 600,
#'          line.width = 5, line.color = "gray44",
#'          scale = "year", scale.format = "\%Y", scale.font = 2, scale.orient = 1,
#'          scale.above = FALSE, scale.cex = 1, scale.tickwidth = 2,
#'          labels = paste(df[[1]], df[[2]]), label.direction = "downup",
#'          label.length = c(0.5,0.5,0.8,0.8), label.position = c(1,3),
#'          label.color = "gray44", label.cex = 0.8, label.font = 1, label.angle = 0,
#'          pch = 20, point.cex = 1, point.color = "gray44")
#' @param df Data frame for events and dates. First column for event names and second column for dates in \code{Date} class.
#' @param main Title of the plot.
#' @param xlab X axis label.
#' @param buffer.days Additional days to add before and after the event dates on the timeline. Default is 600 days.
#' @param line.width Timeline width; default 5
#' @param line.color Timeline color.
#' @param scale Scale on timeline. One of \code{"year","quarter", "month", "week" or "day"}. See \code{\link{seq.Date}}.
#' @param scale.format Scale format; default \code{"\%Y"}.
#' @param scale.font Integer specifying font of scale. Default is 2. (1:plain, 2:bold, 3:italic, 4:bold italic, 5:symbol).
#' @param scale.orient Orientation of scale; default 1(upright)
#' @param scale.above If \code{TRUE}, the scale shows above the line.
#' @param scale.cex Scale font size relative to cex.
#' @param scale.tickwidth Width of scale tick; default 2.
#' @param labels Event labels. Events and corresponding dates as default.
#' @param label.direction Direction of labels from timeline. \code{"downup","updown","up", or "down"}, default is \code{"downup"}. See details.
#' @param label.length Distance of event label from the timeline. Could be a single value or a vector of lengths.
#' Default is c(0.5, 0.5, 0.8, 0.8). See details.
#' @param label.position Integer specifying label positions; default c(1,3). See details.
#' @param label.color Label color(s).
#' @param label.cex Font size(s) of event labels; default 0.8.
#' @param label.font Integer specifying label font; default 1.
#' @param label.angle Angle of text in the label.
#' @param pch End point symbol(s).
#' @param point.cex End points size(s).
#' @param point.color End points color(s).
#' @details
#' \code{label.direction} indicates the direction of event labels from timeline. \code{"downup"} and \code{"updown"} plots
#' alternating labels; \code{"up"} puts all the labels above and \code{"down"} below the timeline.
#'
#' \code{label.length} could be a single number or a numeric vector. For label directions  \code{"downup"} and \code{"updown"}, use between 0 and 0.9, and for
#' \code{"up"} and \code{"down"}, use between 0 and 1.6.
#' For example, \code{label.length = 0.5} produces all the labels at equal lengths, and \code{label.length = c(0.5,0.5,0.8,0.8)}
#' repeats the sequence of lengths.
#'
#' The positions for \code{label.position} are 1: below 2: left 3: above 4: right.
#' @examples
#' ### Default down-up labels
#' timelineS(mj_life, main = "Life of Michael Jackson")
#'
#' ### Labels above timeline and other change in aesthetics
#' timelineS(mj_life, main = "Life of Michael Jackson",
#' label.direction = "up", label.length = c(0.2,0.8,0.4,1.2), label.position = 3,
#' line.color = "blue", label.color = "blue", point.color = "blue", pch = "-")
#'
#' @author Dahee Lee
#' @seealso \code{\link{axis.Date}}, \code{\link{timelineG}}, \code{\link{durCalc}}, \code{\link{durPlot}}
#' @import graphics
#' @export

timelineS <- function(df, main = NA, xlab = NA, buffer.days = 600,
                      line.width = 5, line.color = "gray44",
                      scale = "year", scale.format = "%Y", scale.font = 2, scale.orient = 1,
                      scale.above = FALSE, scale.cex = 1, scale.tickwidth = 2,
                      labels = paste(df[[1]], df[[2]]), label.direction = "downup",
                      label.length = c(0.5,0.5,0.8,0.8), label.position = c(1,3),
                      label.color = "gray44", label.cex = 0.8, label.font = 1, label.angle = 0,
                      pch = 20, point.cex = 1, point.color = "gray44")
{

  if(!is.data.frame(df)){
    stop("'df' must be a data frame")
  }

  # Remove NA rows
  df <- df[rowSums(is.na(df)) == 0,]
  event.names <- df[[1]]
  event.dates <- df[[2]]

  # Change direction and where to put the timeline
  if (label.direction == "downup"){
    d = c(-1, 1)
    h = 0
  } else if (label.direction == "updown"){
    d = c(1, -1)
    h = 0
  } else if (label.direction == "up"){
    d = 1
    h = -0.7
  } else if (label.direction == "down"){
    d = -1
    h = 0.7
  } else {
    d = c(-1, 1)
    h = 0
    print("incorrect label.direction, plot used default")
  }

  # Make line, points and segments
  range.events <- range(min(event.dates) - buffer.days, max(event.dates) + buffer.days)
  r1 <- range.events[1]
  r2 <- range.events[2]
  plot(NA, ylim = c(-1,1), xlim = range.events, ann = FALSE, axes = FALSE)
  title(main = main, xlab = xlab)
  points <- rep_len(d * label.length, length.out = nrow(df))
  events <- rep_len(label.position, length.out = nrow(df))
  segments(event.dates, h, event.dates, points + h, col = label.color)

  # Add Scale
  axis.Date(
    ifelse(scale.above == TRUE, 3, 1),
    at = seq(as.Date(paste0(lubridate::year(r1), "-", lubridate::month(r1), "-", 1)),
             as.Date(paste0(lubridate::year(r2), "-", lubridate::month(r2)+1,"-", 1)), by=scale),
    format = scale.format,
    cex.axis = scale.cex,
    pos = h,
    lwd.tick = scale.tickwidth,
    col = line.color,
    font = scale.font,
    las = scale.orient)

  abline(h = h, lwd = line.width, col = line.color)

  # Add Event Labels
  points(x = event.dates, y = points+h, pch = pch, cex = point.cex, col = point.color)
  text(x = event.dates, y = points+h, labels = labels, cex = label.cex, pos = events, font = label.font, srt = label.angle)
}


