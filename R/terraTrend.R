#' Plot proxy trends
#' @param dirtdata Dataframe of soil/sediment/geologic data.
#' @param depth Character. Name of depth column in dirtdata.
#' @param title Character. Name for the title of the plot. Default = NULL.
#' @param linecolors Character. Specified line colors for the plotted trends. Must be the same length as proxies. Default is NA, in which random colors will be assigned.
#' @param legendtitle Character. Name for the title of the legend. Default = NA.
#' @param value_pos Logical. This argument is set to TRUE, so the x axis and labels will be at the bottom. If you want it at the top, change it to value_pos = FALSE
#' @param legend_pos Logical. This argument is set to TRUE, so it will place the legend at the top. If you want it at bottom, change it to legend_pos = FALSE
#' @param measurement_lab Character. Sets the label for the x-axis.
#' @param depth_lab Character. This argument is set to NA, so it will put the name of your depth column here. However, if you want to change it you can type depth_lab = “Depth(cm)” or whatever you’d like.
#' @param depth_axis Logical. This argument is set to TRUE, so your plot will have the depth label and axis ticks. If you’d like to remove it for a future multipanel plot, then set it to FALSE.
#' @param top_is_zero Logical. This argument is set to TRUE, so 0 is at the top of the graph, and max depth is at the bottom. If you’d like to switch it, type top_is_zero = FALSE.
#' @param depth_intervals Numeric. Interval for plotting depths. The default value of this argument is 10.
#' @param min_depth Numeric. Starting depth for the plot. The default value is 0.
#' @param max_depth Numeric. Sets maximum depth for the y axis of the plot. You must provide a value.
#' @param min_measure Numeric. Adjusts x-axis minimum number.
#' @param max_measure Numeric. Adjusts x-axis maximum number.
#' @param measure_intervals Numeric. Adjusts x-axis intervals.
#' @param proxies Character. Proxies to be plotted. These should be column names in dirtdata and must match exactly.
#' @return A plot of soil proxy trends.
#' @examples
#' #filter for one profile, in this case profile 5
#' data(testPit) 
#' testPitP5 <- dplyr::filter(testPit, ProfileID == "Profile_5")
#' terraP5 <- terraTrend(testPitP5, depth = "Depth_cm_Start", linecolors = c("red","purple"), 
#'               max_depth = 140, proxies = c("Ca","Mg"), measurement_lab = "PPM",legendtitle = 
#'               "Elements", depth_lab = "Depth (cm)")
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @author Gwen Olivier, Samuel R. Borstein
#' @export


terraTrend <- function(dirtdata, depth, title = NULL, linecolors = NA, legendtitle = NA, value_pos = TRUE, legend_pos = TRUE, measurement_lab, depth_lab = NA, depth_axis = TRUE, top_is_zero = TRUE, depth_intervals = 10, min_depth = 0, max_depth, min_measure=NA, max_measure=NA, measure_intervals=NA, proxies) {

  #fix data
  dirtdata[[depth]] <- as.numeric(dirtdata[[depth]]) #need this numeric to plot
  dirtdata <- dirtdata[order(dirtdata[[depth]]), ] #making sure my depth cols are in correct order
  
  #shortcut so I don't have to write out a separate plot for each argument
  dirtcolumns <- proxies
  
  #removes column that are not being used (i.e., col# = NA) so it only plots selected cols
  #dirtcolumns <- dirtcolumns[!is.na(dirtcolumns)]
  
  #need to pivot_longer for this to plot correctly
  longdirtdata <- dirtdata %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(dirtcolumns), names_to = "MeasuredProxy", values_to = "Value") %>%
    dplyr::group_by(.data$MeasuredProxy) %>%
    dplyr::arrange(!!rlang::sym(depth), .by_group = TRUE) %>% #have to use !!sym to call upon a string or this won't work
    dplyr::ungroup() # sorts the groups so it plots by depth, rather than MeasuredProxy driving it
  
  #need to create a dummy row so the plot honors the max_depth argument
  dummy_row <- data.frame(
  MeasuredProxy = dirtcolumns[1], #this needs to be col1 or else it'll think there's another trendline and mess up linecolors
  Value = NA
  )
  
  dummy_row[[depth]] <- max_depth
  dummy_row <- dummy_row[, c(depth, setdiff(names(dummy_row), depth))] #reorder it in the correct way

#bind it to longdirtdata - can't use rbind bc the dummy_row depth col has to have exact name
  longdirtdata <- dplyr::bind_rows(longdirtdata, dummy_row) #need to bind rows for this to work 
  
  #plot it 
  trendplot <- ggplot2::ggplot(data = longdirtdata, ggplot2::aes(x = !!rlang::sym(depth), y = .data$Value, color = .data$MeasuredProxy, group = .data$MeasuredProxy)) + #need !!sym or else it won't plot
    ggplot2::geom_line(na.rm = TRUE) +  #skips NAs and keeps plotting, this will be helpful for Phosphorus trends
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, margin = ggplot2::margin(b = 10)),
      plot.margin = ggplot2::margin(t = 40, r = 10, b = 40, l = 10),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)),
      legend.position = "top"
    ) +
    ggplot2::ggtitle(title) +
    ggplot2::labs(y = measurement_lab)+
    ggplot2::labs(color = legendtitle)
    #scale_x_continuous(breaks = seq(min_depth, max_depth, by = depth_intervals))
  
  #option to adjust the depth label
  if (!is.na(depth_lab)) {
    trendplot <- trendplot +
      ggplot2::labs(x = depth_lab)
  }
  
  #option to remove depth axis and ticks if needed for future multi-panel plots
  if (depth_axis == FALSE) {
    trendplot <- trendplot + ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  }
  
  #linecolors argument, then adding legendtitle. I can't figure out how to do these seprately. So, legendtitle is dependant on linecolors and vice versa
  if (!all(is.na(linecolors))) {
    proxies <- unique(longdirtdata$MeasuredProxy)
    if (length(linecolors) < length(proxies)) {
      stop("Please enter enough colors for each trend line in the linecolor argument :)")
    }
   

    if (is.na(legendtitle)) {
      trendplot <- trendplot + ggplot2::scale_color_manual(values = linecolors, name = "") #blanks out the legend title
    } else {
      trendplot <- trendplot + ggplot2::scale_color_manual(values = linecolors, name = legendtitle)
    }
  }
  
  #option to place the value axis at the top if `value_pos == FALSE`
  if (value_pos == FALSE) {
    trendplot <- trendplot + ggplot2::scale_y_continuous(position = "top")
  }
  
  #moves legend to the bottom if the researcher wants that
  if (legend_pos == FALSE) {
    trendplot <- trendplot + ggplot2::theme(legend.position = "bottom")
  }
  
  if (all(!is.na(c(max_measure, min_measure,measure_intervals)))) { #need to all or it doesn't work. I'm not sure if I want to do an if statement for each one so they can be used independently
  trendplot <- trendplot +
    ggplot2::scale_y_continuous(breaks = seq(min_measure, max_measure, by = measure_intervals))
  } 
  
  #adjust intervals on depth side
  if (top_is_zero == TRUE) {
  trendplot <- trendplot +
    ggplot2::scale_x_reverse(breaks = seq(min_depth, max_depth, by = depth_intervals))
  } else {
  trendplot <- trendplot +
    ggplot2::scale_x_continuous(breaks = seq(min_depth, max_depth, by = depth_intervals))
  }

trendplot <- trendplot + ggplot2::coord_flip()
  
  return(trendplot)

  
}
