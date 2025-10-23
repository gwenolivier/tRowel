#' Prepare your soil/sediment picture for use in paleoPanel
#' @param imagefile Name of file containing a picture of the soils/sediment
#' @param imageformat File format of imagefile. Should be either png or jpg.
#' @param width Numeric. Width for scaling picture for ggplot plotting
#' @param height Numeric. Width for scaling picture for ggplot plotting
#' @param height Numeric. Width for scaling picture for ggplot plotting
#' @param WHratio the width to height ratio of your image. tRowel recommends you look up the pixel width and height of the actual image description
#' @description
#' This function saves your soil/sediment pic as a ggplot object, so when you can combine it with other plots in paleoPlot.
#' @author Gwen Olivier, Samuel R. Borstein
#' 
groundTruth <- function(imagefile, imageformat = "png", width, height, WHratio){

  #upload image
  if(grepl("png",imageformat, ignore.case = T)){
    dirtpic <- png::readPNG(imagefile)
  }
  if(grepl("jpg|jpeg",imageformat, ignore.case = T)){
    dirtpic <- jpeg::readJPEG(imagefile)
  }
  dirtpic <- grid::rasterGrob(dirtpic, interpolate = TRUE, height= grid::unit(1, "npc"))

  #make a blank plot to put image into
  picplot <- ggplot2::ggplot()+
    ggplot2::annotation_custom(dirtpic, xmin = 0, xmax = width, ymin = 0, ymax = height) +
    ggplot2::coord_fixed(ratio = WHratio) +  #so image doesn't distort
    ggplot2::xlim(0, width) +
    ggplot2::ylim(0, height) +
    ggplot2::theme_void()+
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
  
  #message("Please revisit the tutorial if your image is distorted. This can be fixed by cropping your image ahead of time and loading in the correct width, height, and width-height ratio.")
  
  return(picplot)
  
}
