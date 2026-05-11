#' Plots clastCluster data with landscape elevation
#' @param clastData Enter clastCluster data from the saved clastCluster list in environment
#' @param elevData Data frame. Name of the df containing Profile Name and Elevation columns
#' @param surfaceElevM Name of the column containing elevation (meters only) in elevData
#' @param profileClast Name of the profile ID column in clastData
#' @param profileElev Name of the profile ID column in elevData
#' @param depthticksM Numeric. The displayed numeric intervals of the depth axis, the default is 1 meter
#' @param clusterColors Character. Colors to be used in plotting. Default is random soil-like colors.
#' @param title Character. Title of plot
#' @param xLabel Character. Title of profile axis
#' @param yLabel Character. Title of depth axis, the default is "Depth (m)"
#' @details
#' This function plots the data from clastCluster with elevation. You must complete clastCluster before using this function.
#' @returns A cluster plot demonstrating clusters by depth while including the elevation
#' @author Sam Borstein, Gwen Olivier
#' @importFrom dplyr mutate
#' @examples
#' # example code
#' 
#' 
#' 
#' 
#' @export




terraceWrangler <- function(clastData, elevData,surfaceElevM,profileClast, title=NA, profileElev,depthticksM=1, 
                      clusterColors = c("#DCC7AA",
                                        "#A68A6D", 
                                        "#6E5A48", 
                                        "#D29F6C",
                                        "#E8D3A1", 
                                        "#4F4336",
                                        "#AAB8B4",
                                        "#4A615D",
                                        "#B3A79E",
                                        "#8B3A1C",
                                        "#C15629"),
                      title=NULL,xLabel=NULL,yLabel="Depth (m)"){
  names(clastData)[names(clastData) == profileClast] <- "ProfileID"
  names(elevData)[names(elevData) == profileElev] <- "ProfileID"
  minDepth = min(clastData$starts)
  maxDepth = max(clastData$ends)
  
  MergeData <- clastData
  MergeData$surfaceElev <- NA
  for(i in 1:length(unique(elevData$ProfileID))){
    hits <- which(MergeData$ProfileID==unique(elevData$ProfileID)[i])
    MergeData$surfaceElev[hits] <- elevData$surfaceElevM[i]
  }
  
  
  plotE <- MergeData %>% 
    mutate(elevTop = .data$surfaceElev, #elevTop becomes the new depth column
    )
  
  plotF <- plotE %>%
    mutate(elevStart = .data$elevTop-(.data$starts/100),
           elevEnd = .data$elevTop-(.data$ends/100))
  #then copy over plot code from clastCluster, but theres a lot of maxdepth and mindepth in plot code, 
  #so I am wondering if we need a different set up


p <- ggplot2::ggplot(data = plotF, ggplot2::aes(x=.data$elevStart,y=.data$ProfileID,color = factor(.data$cluster))) +#initiate ggplot
   #set up x-axis
  ggplot2::scale_y_discrete(limits = levels(plotF$ProfileID))+
  ggplot2::geom_segment(ggplot2::aes(x = .data$elevStart, y = .data$ProfileID, xend = .data$elevEnd, yend = .data$ProfileID, colour = factor(.data$cluster)), data = plotF, linewidth = 5)+#segment drawing
  ggplot2::scale_color_manual(values = clusterColors, aesthetics = c("color"),name = "Cluster")+#make colors what you want
  ggplot2::guides(size = "none", color = ggplot2::guide_legend(override.aes = list(linewidth = 5))) + #rescale segment in legend
  ggplot2::theme_minimal()+
  ggplot2::ggtitle(title) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, margin = ggplot2::margin(b = 10)),
    plot.margin = ggplot2::margin(t = 40, r = 10, b = 40, l = 10),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)))+
    ggplot2::labs(x = yLabel)+
  ggplot2::labs(y = xLabel)+
  ggplot2::coord_flip()


print(p)

}
