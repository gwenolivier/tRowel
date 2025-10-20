groundTruth <- function(imagefile, width, height, WHratio){

  #upload image
  dirtpic <- png::readPNG(imagefile)
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
