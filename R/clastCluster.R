#' Cluster and graphically display soil data
#' @param dirtdata Data frame containing soil characteristic data for clustering.Should have unique rownames.
#' @param distance Character. Either Euclidean or Gower. If you have only numeric data, use Euclidean. If your data contains a mix of data types, use Gower.
#' @param depth Character. Name of depth column in dirtdata.
#' @param profile Character. Name of column containing profile IDs if multiple soil profiles exist in dirtdata. If a single profile, set to Null. Default is NULL (i.e. a single profile).
#' @param elevation Character. Name of column containing information on the elevation.
#' @param ordination Logical. Should an ordination of the data be performed and plotted. Default is FALSE. If distance = Euclidean, Principle Component Analysis (PCA) is performed. If distance = Gower, a Principle Co-ordinate Analysis (PCoA) is performed.
#' @param minK Numeric. Minimum number of clusters to try.
#' @param maxK Numeric. Maximum number of clusters to try.Must be less than the number of rows in dirtdata.
#' @param nstart Numeric. Number of random starts to be used in clustering algorithm.
#' @param plot Logical. Should plots be generated. Default is TRUE. 
#' @param depthticks Numeric. Interval for plotting depth. Default is 10.
#' @param minDepth Numeric. Minimum depth to be used in plotting. Default is 0.
#' @param maxDepth Numeric. Maximum depth to be used in plotting. Default is NULL.
#' @param clusterColors Character. Colors to be used in plotting.
#' @param profileOrder Character. Order in which to plot profiles if multiple soil profiles are present in dirtdata.
#' @param profileName Character. If a single profile. What should profile be named? Default is Null.
#' @param title Character. Title for ordination plots. Default is NULL.
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @return A list Containing the data and plots.
#' @examplesIf interactive()
#' \donttest{
#' load(testpit)
#' #Prep data
#' testPit3 <- testPit
#' testPit3 <- select(testPit3, c(-Munsell ))
#' 
#' #Example using Euclidean distance and PCA
#' EuclidExample <- clastCluster(dirtdata = testPit3, distance = "E",depth = "Depth_cm",
#' profile = "ProfileID",ordination = TRUE,plot = TRUE,maxDepth=110)
#' 
#' #Example using Gower's distance and PCoA
#' GowerExample <- clastCluster(dirtdata = testPit3, distance = "G",depth = "Depth_cm",
#' profile = "ProfileID",plot = TRUE, minK = 2, maxK=8, title="Cluster Analysis of Arroyo Profiles",
#'  ordination = T)
#' }
#' @export
#' @author Gwen Olivier, Samuel R. Borstein

clastCluster <- function(dirtdata, distance, depth, profile=NULL, elevation=NULL, ordination=FALSE, minK=2, maxK=10, nstart=50, plot=TRUE, depthticks=10, minDepth=0, maxDepth=NULL, clusterColors = c("#DCC7AA",
  "#A68A6D", 
  "#6E5A48",  
  "#4F4336",
  "#E8D3A1", 
  "#D29F6C",
  "#AAB8B4",
  "#4A615D",
  "#B3A79E",
  "#8B3A1C",
  "#C15629"), profileOrder=NULL, profileName = NULL,title=NULL){
  
#prep data
  #dirtdata[[depth]] <- as.factor(dirtdata[[depth]]) #need this as a factor to plot later
  
  dirtdata2 <- dplyr::select(dirtdata, -dplyr::all_of(depth))
  
  if (!is.null(profile)) {
    dirtdata2 <- dplyr::select(dirtdata2, -dplyr::all_of(profile))
  }
  
  if (!is.null(elevation)) {
    dirtdata2 <- dplyr::select(dirtdata2, -dplyr::all_of(elevation))
  }
  
  
#Cluster Analysis part of function
  if(distance %in% c("E","e","Euclidean","euclidean")){
    dirtdata2 %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), scale))
    Avg_Sil <- factoextra::fviz_nbclust(dirtdata2, stats::kmeans, method = "silhouette", k.max = maxK) #cluster recommendation 

  print(Avg_Sil) 
  
  #ask user to input number of clusters
  k <- readline(prompt = "Type the number of clusters for your analysis in console and hit enter ")

  #convert to numeric or else it'll be read as a string
  k <- as.numeric(k)
  
  km.res <- stats::kmeans(dirtdata2, k, nstart = nstart) 
  
  if(ordination==TRUE){
  
 pcaPlot <- factoextra::fviz_cluster(km.res, data = dirtdata2, geom = "point")
 
 pca_df <- pcaPlot$data
 
 pca_df$depth <- dirtdata[[depth]]
 
 pcaPlot2 <- ggplot2::ggplot(pca_df, ggplot2::aes(x = .data$x, y = .data$y, color = .data$cluster)) +
  ggplot2::geom_point() +
  ggplot2::geom_text(ggplot2::aes(label = depth), vjust = -0.5)

print(pcaPlot2)

  }
  
  dirtdata$cluster <- km.res$cluster
  
  }
  
if(distance %in% c("G","Gower","g","gower")){ 
 
  #calc Gower's distance
  MyDist <- gawdis::gawdis(dirtdata2)
  
 #Function to mine K values. Just a loop.
#Will take silhoute widths,  is an aggregated measure of how similar an observation 
#is to its own cluster compared its closest neighboring cluster.
#Higher values are better, ranges from -1:1.
#... argument is used to pass arguments from others. See they ar in plot, which means it passes
#arguments from plot in R and we don't have to specify them. So you can cal pch, color, etc.
MineClusters <- function(distance, GminK, GmaxK, PlotSil = TRUE, ...){
  K_Range <- GminK:GmaxK#Get range of K to try
  sil_width <- vector(mode = "numeric",length = length(K_Range))#Create empty vector to store results
  
  #Run loop trying various k-values (must be n-1 number of observations)
  for(i in 1:length(K_Range)){
    
    pam_fit <- cluster::pam(x = distance,
                   diss = TRUE,
                   k = K_Range[i])
    
    sil_width[i] <- pam_fit$silinfo$avg.width
    names(sil_width) <- paste0("K_",K_Range)
    
  }
  if(PlotSil == TRUE){
    plot(K_Range, sil_width,
         xlab = "Number of clusters",
         ylab = "Silhouette Width", type = "b", ...)
    graphics::abline(v = 1+which(sil_width==max(sil_width)), lty = 2)
  }
  return(sil_width)
}
#Average silhouette 
Avg_Sil <- MineClusters(distance = MyDist,GminK = minK,GmaxK = maxK,PlotSil = TRUE, pch = 17, lwd = 2, col = "black")

k_num <- readline(prompt = "Type the number of clusters for your analysis in console and hit enter ")
 
#convert to numeric or else it'll be read as a string
  k_num <- as.numeric(k_num)
   
  #Run clusters
clusters <- cluster::pam(x = MyDist, k = k_num, diss = TRUE, nstart = nstart)#generates the clusters
#clusters$clustering#see clusters
dirtdata$cluster <- clusters$clustering

#Ordinate with PCoA Which can handle a distance/dissimilarity matrix
res <- ape::pcoa(MyDist)
pcoa.dat <- data.frame(res$vectors) 
pcoa.dat$clusters <- clusters$clustering

#Generate hulls for plotting polygons
hull <- pcoa.dat %>% dplyr::group_by(.data$clusters) %>% 
  dplyr::slice(grDevices::chull(.data$Axis.1,.data$Axis.2))

#generate plot for pcoa
pcaPlot <-  ggplot2::ggplot(ggplot2::aes(x = .data$Axis.1, y = .data$Axis.2), data = pcoa.dat) +
  ggplot2::geom_point(ggplot2::aes(color = as.factor(.data$clusters)),size = 2)+
  ggplot2::theme_minimal()+
  ggplot2::geom_polygon(data = hull, alpha = 0.3, 
                        ggplot2::aes(fill = as.factor(.data$clusters),colour = as.factor(.data$clusters)))+
  ggplot2::guides(color = ggplot2::guide_legend(title = "Cluster"),
         fill = ggplot2::guide_legend(title = "Cluster"))+
  ggplot2::labs(title = title, x = paste0("PC1 (",round(res$values$Rel_corr_eig[1],4)*100,"%)"),
       y = paste0("PC2 (",round(res$values$Rel_corr_eig[2],4)*100,"%)"))

print(pcaPlot)
}

#Plotting time
if (plot==TRUE){
  
 dirtdata <- dirtdata %>% 
  dplyr::mutate(starts = .data[[depth]],
    ends   = .data[[depth]] + depthticks)
 
 dirtdata <- dplyr::select(dirtdata, -dplyr::all_of(depth))
 
 if(!is.null(profile) && !is.null(profileOrder)){ #order profiles at bottom of plot
 dirtdata[[profile]] <- factor(dirtdata[[profile]],
                                    levels = profileOrder)
 }
  #maxDepth
    if (is.null(maxDepth)) {
      maxDepth <- max(dirtdata$ends, na.rm = TRUE)
    }
 
 
 #need to do two ggplots (without profile too)
 
 if(!is.null(profile)){
 
   p <- ggplot2::ggplot(dirtdata, ggplot2::aes(x=.data$starts,y=.data[[profile]],color = factor(.data$cluster))) +#initiate ggplot
  ggplot2::scale_x_reverse(limits = c(minDepth, maxDepth),name = "Depth (cm)", breaks = seq(minDepth, maxDepth, by = 10))+ #set up x-axis
  ggplot2::scale_y_discrete(limits = levels(dirtdata[[profile]]))+
  ggplot2::geom_segment(ggplot2::aes(x = .data$starts, y = .data[[profile]], xend = .data$ends, yend = .data[[profile]], colour = factor(.data$cluster)), data = dirtdata, linewidth = 5)+#segment drawing
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
    ggplot2::coord_flip()
   
   
   print(p)
 }else{
   dirtdata$profile_fill <- factor(profileName) #use profileName argument to add a label on y
    
  p<- ggplot2::ggplot(dirtdata, ggplot2::aes(x=.data$starts, .data$profile_fill, color = factor(.data$cluster))) +#initiate ggplot
  ggplot2::scale_x_reverse(limits = c(minDepth, maxDepth),name = "Depth (cm)", breaks = seq(minDepth, maxDepth, by = 10))+ #set up x-axis
  ggplot2::scale_y_discrete(limits = levels(dirtdata$profile_fill))+
  ggplot2::geom_segment(ggplot2::aes(x = .data$starts, y = .data$profile_fill, xend = .data$ends, yend = .data$profile_fill, colour = factor(.data$cluster)), data = dirtdata, linewidth = 5)+#segment drawing
  ggplot2::scale_color_manual(values = clusterColors, aesthetics = c("color"),name = "Cluster")+#make colors what you want
  ggplot2::guides(size = "none", color = ggplot2::guide_legend(override.aes = list(linewidth = 10))) + #rescale segment in legend
    ggplot2::theme_minimal()+
    ggplot2::ggtitle(title) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, margin = ggplot2::margin(b = 10)),
    plot.margin = ggplot2::margin(t = 40, r = 10, b = 40, l = 10),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)))+
    ggplot2::coord_flip()

    
  
  print(p)
 }
 
 plots <- list(
    main = p,
    silhouette = Avg_Sil
  )

  if (ordination) {
    plots$ordination <- pcaPlot
  } 
 
 return(list(
    data  = dirtdata,
    plots = plots
  ))
  
}}
