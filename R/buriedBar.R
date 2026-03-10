#' Plots proxies with a bar plot
#' @param pollendata Dataframe of paleoenvironmental data.
#' @param proxies Columns to be plotted.
#' @param depth Character. Name of column containing depth data
#' @param title Character. Name of plot.
#' @param barcolors Character Vector. Assigned colors for bar plot.
#' @param legendtitle Character. Title of legend in plot.
#' @param value_pos Boolean. TRUE places X-axis labels at the bottom of the plot
#' @param legend_pos Boolean. TRUE places the legend at the top of plot 
#' @param measurement_lab Character. Label for the X-axis
#' @param depth_lab Character. Label for the Y-axis. Default is the name of the depth column.
#' @param depth_axis Should the depth axis be plotted. Default is TRUE.
#' @param top_is_zero Boolean. If TRUE, the top of the plot will have a depth of 0. If FALSE, the top of plot will be max depth.
#' @param depth_intervals Numeric. Sets the intervals for the Y-axis. Default is 10.
#' @param min_depth Numeric. Minimum depth of the dataset. Default is 0.
#' @param max_depth Numeric. Required maximum depth for the Y-axis. Allows the user to extend the graph to match other depths in paleoPanel(). For a shorter graph, the user must filter the data.
#' @param min_measure Numeric. Adjusts X-axis min number.
#' @param max_measure Numeric. Adjusts X-axis max number.
#' @param measure_intervals Numeric. Adjusts X-axis intervals.
#' @param panel_format Format for plotting. Can be either panel or single. If single, elements are all plotted on the same plot. If panel, the plot will be constructed as a facet plot. Default is single.

#' @details
#' This function plots a side-by-side bar graph to analyze and compare the quantity of pollen (or other proxy) by depth.
#' @returns A bar plot of pollen amounts.
#' @author Gwen Olivier
#' @importFrom magrittr %>%
#' @examples
#' # example code
#' data("testPit")
#' buriedBar(testPit, proxies = c("Ca","Mg"),depth = "Depth_cm",measurement_lab = "PPM",
#' max_depth = 120,top_is_zero = TRUE, barcolors = c("blue","green"))
#' @export

buriedBar <- function(pollendata, proxies, depth, title=NULL, barcolors = NA, legendtitle = NA, value_pos = TRUE, legend_pos = TRUE, measurement_lab, depth_lab = NA, depth_axis = TRUE, top_is_zero = TRUE, depth_intervals = 10, min_depth = 0, max_depth, min_measure=NA, max_measure=NA, measure_intervals=NA, panel_format = "single") {
  
  #fix data
  pollendata[[depth]] <- as.numeric(pollendata[[depth]])
  pollendata <- pollendata[order(pollendata[[depth]]), ]
  
  #lists of columns
  pollencolumns <- proxies
  

  #clean data with pivot
  longdirtdata <- pollendata %>%
    tidyr::pivot_longer(cols = dplyr::all_of(pollencolumns), names_to = "MeasuredProxy", values_to = "Value") %>%
    dplyr::group_by(.data$MeasuredProxy) %>%
    dplyr::arrange(!!rlang::sym(depth), .by_group = TRUE) %>%
   dplyr::ungroup()  # sorts the groups so it plots by depth, rather than MeasuredProxy driving it


  
  #need to create a dummy row so the plot honors the max_depth argument
  dummy_row <- data.frame(
  MeasuredProxy = pollencolumns[1], #this needs to be col1 or else it'll think there's another trendline and mess up linecolors
  Value = NA
  )
  
  dummy_row[[depth]] <- max_depth
  dummy_row <- dummy_row[, c(depth, setdiff(names(dummy_row), depth))] #reorder it in the correct way
  
  
  
  
   #bind it
  longdirtdata <- dplyr::bind_rows(longdirtdata, dummy_row)  
  
 
  #plot it
  pollenplot <- ggplot2::ggplot(longdirtdata, ggplot2::aes(x = !!rlang::sym(depth),y = .data$Value,fill = .data$MeasuredProxy,group = .data$MeasuredProxy)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) +  #places bars next to eachother
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, margin = ggplot2::margin(b = 10)), #format figure
      plot.margin = ggplot2::margin(t = 40, r = 10, b = 40, l = 10),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)),
      legend.position = "top"
    ) +
    ggplot2::ggtitle(title) +
    ggplot2::labs(y = measurement_lab) +
    ggplot2::scale_x_continuous(
      breaks = seq(min_depth, max_depth, by = depth_intervals),
      limits = c(min_depth, max_depth)
    )
  
  
  #depth label
  if (!is.na(depth_lab)) {
    pollenplot <- pollenplot +
      ggplot2::labs(x = depth_lab)
  }
  
  #remove depth axis and ticks if needed for future multi-panel plots
  if (depth_axis == FALSE) {
    pollenplot <- pollenplot + ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  }

 
  #barcolors and legend title
   if (!all(is.na(barcolors))) {
  proxiesLegend <- unique(longdirtdata$MeasuredProxy)
  
  if (length(barcolors) < length(proxiesLegend)) {
    stop("Please enter enough colors for bar groups.")
  }
  
  if (is.na(legendtitle)) {
    pollenplot <- pollenplot + 
      ggplot2::scale_fill_manual(values = barcolors, name = "")
  } else {
    pollenplot <- pollenplot + 
      ggplot2::scale_fill_manual(values = barcolors, name = legendtitle)
  }
}

   
  #option to place the values at the top if `value_pos == FALSE`
  if (value_pos == FALSE) {
    pollenplot <- pollenplot + ggplot2::scale_y_continuous(position = "top")
  }
  
  #move legend position if needed
  if (legend_pos == FALSE) {
    pollenplot <- pollenplot + ggplot2::theme(legend.position = "bottom")
  }

  #adjust y-axis breaks for measurements if defined
  if (all(!is.na(c(max_measure, min_measure, measure_intervals)))) {
    pollenplot <- pollenplot +
      ggplot2::scale_y_continuous(breaks = seq(min_measure, max_measure, by = measure_intervals))
  }

  #adjust intervals on depth side
  if (top_is_zero == TRUE) {
    pollenplot <- pollenplot +
      ggplot2::scale_x_reverse(breaks = seq(min_depth, max_depth, by = depth_intervals))
  } else {
    pollenplot <- pollenplot +
      ggplot2::scale_x_continuous(breaks = seq(min_depth, max_depth, by = depth_intervals))
  }

#Facet_wrap option! Switched out the word facet for panel so it's more intuitive for non R folks
if (panel_format == "panel") {
 pollenplot <- pollenplot + 
    ggplot2::facet_wrap(~ .data$MeasuredProxy, scales = "free_y") +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(size = 12),
      axis.title.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),  
      axis.text.x = ggplot2::element_blank(),   
      axis.text.y = ggplot2::element_text(size = 10),
      legend.position = "none")+
      ggplot2::guides(fill = "none") 
}
    
  #flip for paleo data
  pollenplot <- pollenplot + ggplot2::coord_flip()

  return(pollenplot)
}
