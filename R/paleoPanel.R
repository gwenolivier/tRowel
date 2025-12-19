#' Plots multiple existing figures/plots to create a panel of figures
#' @param ... fig. This argument provides unlimited space to upload as many figure/plot names as needed. They plot in the order that they are uploaded.
#' @param profile fig. Input the figure name created in groundTruth to add a stratigraphic photo to the front of the panel figure 
#' @param rows Numeric. Number of rows for the panel figure, defaults to 1 row. 
#' @param cols Numeric. Number of columns to format the panel figure. If you are using 1 row, make sure this is equal to the number of plots inputted 
#' @param label_angle Numeric. The angle of X axis labels
#' @param border Boolean. If TRUE, the panel figure will have a border
#' @param marginsize Numeric. Adjusts the margin size between the figures
#' @param plotwidths Numeric Vector. Assists in adjusting the spacing when the profile argument is not NA. Input two numbers (i.e., plotwidths = c(1,5)), the first number reflects the width of the profile argument, and the second number reflects the width of panel figures.
#' @details
#' This function plots multiple existing figures/plots to create a multi-figure panel plot.
#' The user needs to upload figure names (with correct y-axis labels [i.e., some may only want the first figure label to have depth]) 
#' If adding a profile figure, the groundTruth function must be performed first
#' @returns A figure that formats multiple plots 
#' @author Gwen Olivier
#' @examples
#' paleoPanel(figure1, figure2, figure3, cols = 3, label_angle = 45, border = TRUE)


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
