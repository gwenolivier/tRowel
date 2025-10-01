#needed libraries
library(ggplot2)
library(tidyverse)
library(rlang)

#assign arguments to function 
terraTrend <- function(dirtdata, depth, title, linecolors = NA, legendtitle = NA, value_pos = TRUE, fig_width= 8, fig_height = 8, legend_pos = TRUE, measurement_lab, depth_lab = NA, depth_axis = TRUE, top_is_zero = TRUE, depth_intervals = 10, min_depth = 0, max_depth, min_measure=NA, max_measure=NA, measure_intervals=NA, col1, col2 = NA, col3 = NA, col4 = NA, col5 = NA, col6 = NA, col7 = NA, col8 = NA, col9 = NA, col10 = NA, col11 = NA, col12 = NA, col13 = NA) {
  
  #set figure height: first save the user's current set up 
   original_setting <- knitr::opts_current$get()

  #now override it for the plot
  knitr::opts_current$set(fig.width = fig_width, fig.height = fig_height)
  
  #fix data
  dirtdata[[depth]] <- as.numeric(dirtdata[[depth]]) #need this numeric to plot
  dirtdata <- dirtdata[order(dirtdata[[depth]]), ] #making sure my depth cols are in correct order
  
  #shortcut so I don't have to write out a separate plot for each argument
  dirtcolumns <- c(col1, col2, col3, col4, col5, col6, col7, col8, col9, col10, col11, col12, col13)
  
  #removes column that are not being used (i.e., col# = NA) so it only plots selected cols
  dirtcolumns <- dirtcolumns[!is.na(dirtcolumns)]
  
  #need to pivot_longer for this to plot correctly
  longdirtdata <- dirtdata %>%
    pivot_longer(cols = all_of(dirtcolumns), names_to = "MeasuredProxy", values_to = "Value") %>%
    group_by(MeasuredProxy) %>%
    arrange(!!sym(depth), .by_group = TRUE) %>% #have to use !!sym to call upon a string or this won't work
    ungroup() # sorts the groups so it plots by depth, rather than MeasuredProxy driving it
  
  #need to create a dummy row so the plot honors the max_depth argument
  dummy_row <- data.frame(
  MeasuredProxy = col1, #this needs to be col1 or else it'll think there's another trendline and mess up linecolors
  Value = NA
)
  dummy_row[[depth]] <- max_depth
  dummy_row <- dummy_row[, c(depth, setdiff(names(dummy_row), depth))] #reorder it in the correct way

#bind it to longdirtdata - can't use rbind bc the dummy_row depth col has to have exact name
  longdirtdata <- bind_rows(longdirtdata, dummy_row) #need to bind rows for this to work 
  
  #plot it 
  trendplot <- ggplot(longdirtdata, aes(x = !!sym(depth), y = Value, color = MeasuredProxy, group = MeasuredProxy)) + #need !!sym or else it won't plot
    geom_line(na.rm = TRUE) +  #skips NAs and keeps plotting, this will be helpful for Phosphorus trends
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, margin = margin(b = 10)),
      plot.margin = margin(t = 40, r = 10, b = 40, l = 10),
      axis.title.y = element_text(margin = margin(r = 20)),
      axis.text.y = element_text(margin = margin(r = 20)),
      axis.title.x = element_text(margin = margin(t = 20)),
      legend.position = "top"
    ) +
    ggtitle(title) +
    labs(y = measurement_lab)+
    scale_x_continuous(breaks = seq(min_depth, max_depth, by = depth_intervals))
  
  #option to adjust the depth label
  if (!is.na(depth_lab)) {
    trendplot <- trendplot +
      labs(x = depth_lab)
  }
  
  #option to remove depth axis and ticks if needed for future multi-panel plots
  if (depth_axis == FALSE) {
    trendplot <- trendplot + theme(
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
      trendplot <- trendplot + scale_color_manual(values = linecolors, name = "") #blanks out the legend title
    } else {
      trendplot <- trendplot + scale_color_manual(values = linecolors, name = legendtitle)
    }
  }
  
  #option to place the value axis at the top if `value_pos == FALSE`
  if (value_pos == FALSE) {
    trendplot <- trendplot + scale_y_continuous(position = "top")
  }
  
  #moves legend to the bottom if the researcher wants that
  if (legend_pos == FALSE) {
    trendplot <- trendplot + theme(legend.position = "bottom")
  }
  
if (all(!is.na(c(max_measure, min_measure,measure_intervals)))) { #need to all or it doesn't work. I'm not sure if I want to do an if statement for each one so they can be used independently
  trendplot <- trendplot +
    scale_y_continuous(breaks = seq(min_measure, max_measure, by = measure_intervals))
} 
  
  #adjust intervals on depth side
 if (top_is_zero == TRUE) {
  trendplot <- trendplot +
    scale_x_reverse(breaks = seq(min_depth, max_depth, by = depth_intervals))
} else {
  trendplot <- trendplot +
    scale_x_continuous(breaks = seq(min_depth, max_depth, by = depth_intervals))
}

trendplot <- trendplot + coord_flip()
  
  #send a message about fixing the margins on your own 
  message(paste(
    "You can adjust the plot's margins by saving your figure (<-) and then adding + theme()",
    "Here is an example: fig1 <- terraTrend(...)",
    "#now adjust the margins using theme(): fig1 + theme(plot.margin = margin(t = 20, r = 10, b = 10, l = 10), plot.title = element_text(hjust = 0.5, size = 15))",
    "#display figure: fig1",
    "Happy coding! Please cite this package :)",
    sep = "\n"
  ))
  
  return(trendplot)
  
  #reset old figure height/width so this function doesn't mess with other chunks
  knitr::opts_current$set(original_setting)
  
}

#i think max depth is working...?
