


terraTrend <- function(dirtdata, depth, title, linecolors = NA, legendtitle = NA, value_pos = TRUE, fig_width= 8, fig_height = 8, legend_pos = TRUE, measurement_lab, depth_lab = NA, depth_axis = TRUE, top_is_zero = TRUE, depth_intervals = 10, min_depth = 0, max_depth, min_measure=NA, max_measure=NA, measure_intervals=NA, proxies) {

  #fix data
  dirtdata[[depth]] <- as.numeric(dirtdata[[depth]]) #need this numeric to plot
  dirtdata <- dirtdata[order(dirtdata[[depth]]), ] #making sure my depth cols are in correct order
  
  #shortcut so I don't have to write out a separate plot for each argument
  dirtcolumns <- proxies
  
  #removes column that are not being used (i.e., col# = NA) so it only plots selected cols
  #dirtcolumns <- dirtcolumns[!is.na(dirtcolumns)]
  
  #need to pivot_longer for this to plot correctly
  longdirtdata <- dirtdata %>%
    tidyr::pivot_longer(cols = all_of(dirtcolumns), names_to = "MeasuredProxy", values_to = "Value") %>%
    dplyr::group_by(MeasuredProxy) %>%
    dplyr::arrange(!!sym(depth), .by_group = TRUE) %>% #have to use !!sym to call upon a string or this won't work
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
  trendplot <- ggplot2::ggplot(longdirtdata, aes(x = !!sym(depth), y = Value, color = MeasuredProxy, group = MeasuredProxy)) + #need !!sym or else it won't plot
    ggplot2::geom_line(na.rm = TRUE) +  #skips NAs and keeps plotting, this will be helpful for Phosphorus trends
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = element_text(hjust = 0.5, margin = margin(b = 10)),
      plot.margin = margin(t = 40, r = 10, b = 40, l = 10),
      axis.title.y = element_text(margin = margin(r = 20)),
      axis.text.y = element_text(margin = margin(r = 20)),
      axis.title.x = element_text(margin = margin(t = 20)),
      legend.position = "top"
    ) +
    ggplot2::ggtitle(title) +
    ggplot2::labs(y = measurement_lab)#+
    #scale_x_continuous(breaks = seq(min_depth, max_depth, by = depth_intervals))
  
  #option to adjust the depth label
  if (!is.na(depth_lab)) {
    ggplot2::trendplot <- trendplot +
      labs(x = depth_lab)
  }
  
  #option to remove depth axis and ticks if needed for future multi-panel plots
  if (depth_axis == FALSE) {
    trendplot <- trendplot + ggplot2::theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
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
