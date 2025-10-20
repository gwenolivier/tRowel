



                      #... allows for an infinite amount of plots
paleoPanel <- function(..., profile=NULL, rows = 1, cols, label_angle = 0, border = FALSE, bordercolor = "darkgrey", marginsize = 0, soilpic = FALSE, plotwidths=NULL) {
  
  figs <- list(...)  #store figures
  
  #use lapply to adjust each figure with a function. The function sorta acts like ggplot where I can add conditions 
  figs <- lapply(figs, function(fixplots) {
    
    #fix labels if needed
    fixplots <- fixplots + ggplot2::theme( #have to save to fixplots or the changes wont stay
      axis.text.x = ggplot2::element_text(
        angle = label_angle,
        hjust = ifelse(label_angle == 90, 0.5, ifelse(label_angle == 45, 1, 0.5)) #adjusts the x axis if it's not readable. This statement checks if the label angle is 45, if so it adjusts the alignment of the numbers so it looks nice diaganoly and shifts a little left. If it's not 45 then the text stays center to the tick marks  
      )
    )
    
    #option to adjust margins
    fixplots <- fixplots + ggplot2::theme(
      plot.margin = ggplot2::margin(0, 0, 20, marginsize)
    )
    
    return(fixplots) #need to do this or it won't save correctly
  })
  
  #adds borders to all but profile argument 
 if (border == TRUE) {
    figs <- lapply(figs, function(fixplots) {
      fixplots + ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
                           color = bordercolor, fill = NA, linewidth = 0.75)
    })
  }

  
if(!is.null(profile)) {
  profile <- profile +
     ggplot2::theme(
      axis.title.x =  ggplot2::element_blank(),
       axis.text.x =  ggplot2::element_blank(),
       axis.ticks.x =  ggplot2::element_blank(),
       plot.margin =  ggplot2::margin(0, 0, 0, 0) 
     )
}
  
  
  
  #combine all the plots using wrap_plots() from patchwork
main_panel <- patchwork::wrap_plots(figs, ncol = cols, nrow = rows)

#if profile (the soil pic) is entered, place it on the left
if (!is.null(profile)) {
  
  profile <- profile +
     ggplot2::coord_fixed() +
     ggplot2::theme(
      axis.title.x =  ggplot2::element_blank(),
      axis.text.x =  ggplot2::element_blank(),
      axis.ticks.x =  ggplot2::element_blank(),
      plot.margin =  ggplot2::margin(0, 0, 0, 0) #hopefully fixed the margins
    )
  
  #default plotwidths if argument is NULL and profile has something it
  if (is.null(plotwidths) || anyNA(plotwidths)) {
    plotwidths <- c(1, 3)
  }

  #only use plot_layout() if profile is being used
  panel <- profile + main_panel + patchwork::plot_layout(ncol = 2, widths = plotwidths)

} else {
  #otherwise return the main_panel from above, needed to move positioning of arguments for this to work.
  panel <- main_panel
}

#message for help page("Your plots could look funky in R or knitted Markdown, make sure you export figure for it to look normal. You may need to tinker with the dimensions in export or markdown file. ")
  return(panel)


}
