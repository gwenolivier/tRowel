
library(factoextra)
library(gawdis)
library(tidyverse)
library(magrittr)
library(cluster)

# **Function code:**

clastCluster <- function(dirtdata, distance, depth, profile=NULL, elevation=NULL, PCA=FALSE, minK=2, maxK=10, nstart=50, plot=TRUE, interval=10, minDepth=0, maxDepth=NULL, clusterColors = c("#DCC7AA",
  "#A68A6D", 
  "#6E5A48",  
  "#4F4336",
  "#E8D3A1", 
  "#D29F6C",
  "#AAB8B4",
  "#4A615D",
  "#B3A79E",
  "#8B3A1C",
  "#C15629"), profileOrder=NULL, profileName = NULL, plotElevation = FALSE,title=NULL){
  
#prep data
  dirtdata[[depth]] <- as.factor(dirtdata[[depth]]) #need this as a factor to plot later
  
  dirtdata2 <- dplyr::select(dirtdata, -dplyr::all_of(depth))
  
  if (!is.null(profile)) {
    dirtdata2 <- dplyr::select(dirtdata2, -dplyr::all_of(profile))
  }
  
  if (!is.null(elevation)) {
    dirtdata2 <- dplyr::select(dirtdata2, -dplyr::all_of(elevation))
  }
  
  
#Cluster Analysis part of function
  if(distance == "E"){
    dirtdata2 %<>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), scale))
    silPlot <- factoextra::fviz_nbclust(dirtdata2, kmeans, method = "silhouette", k.max = maxK) #cluster recommendation 

  print(silPlot) 
  
  #ask user to input number of clusters
  k <- readline(prompt = "Type the number of clusters for your analysis in console and hit enter ")

  #convert to numeric or else it'll be read as a string
  k <- as.numeric(k)
  
  km.res <- kmeans(dirtdata2, k, nstart = nstart) 
  
  if(PCA==TRUE){
  
 pcaPlot <- factoextra::fviz_cluster(km.res, data = dirtdata2, geom = "point")
 
 pca_df <- pcaPlot$data
 
 pca_df$depth_lab <- dirtdata[[depth]]
 
 pcaPlot2 <- ggplot2::ggplot(pca_df, ggplot2::aes(x = x, y = y, color = cluster)) +
  ggplot2::geom_point() +
  ggplot2::geom_text(ggplot2::aes(label = depth_lab), vjust = -0.5)

print(pcaPlot2)

  }
  
  dirtdata$cluster <- km.res$cluster
  
  }
  
if(distance == "G"){ 
 
  #calc Gower's distance
  MyDist <- gawdis(dirtdata2)
  
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
    #abline(v = 1+which(Avg_Sil==max(Avg_Sil)), lty = 2)
  }
  return(sil_width)
}
#Average silhouette 
Avg_Sil <- MineClusters(distance = MyDist,GminK = minK,GmaxK = maxK,PlotSil = TRUE, pch = 17, lwd = 2, col = "purple")

k_num <- readline(prompt = "Type the number of clusters for your analysis in console and hit enter ")
 
#convert to numeric or else it'll be read as a string
  k_num <- as.numeric(k_num)
   
  #Run clusters
clusters <- pam(x = MyDist, k = k_num, diss = TRUE)#generates the clusters
clusters$clustering#see clusters

dirtdata$cluster <- clusters$clustering

}


#Plotting time
if (plot==TRUE){
  
 dirtdata <- dirtdata %>% 
  dplyr::mutate(starts = as.numeric(.data[[depth]]),
    ends   = as.numeric(.data[[depth]]) + interval)
 
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
 
   p <- ggplot2::ggplot(dirtdata, ggplot2::aes(x=starts,y=.data[[profile]],color = factor(cluster))) +#initiate ggplot
  ggplot2::scale_x_continuous(limits = c(minDepth, maxDepth),name = "Depth (cm)")+ #set up x-axis
  ggplot2::scale_y_discrete(limits = levels(dirtdata[[profile]]))+
  ggplot2::geom_segment(ggplot2::aes(x = starts, y = .data[[profile]], xend = ends, yend = .data[[profile]], colour = factor(cluster)), data = dirtdata, linewidth = 5)+#segment drawing
  ggplot2::scale_color_manual(values = clusterColors, aesthetics = c("color"),name = "Cluster")+#make colors what you want
  ggplot2::guides(size = "none", color = ggplot2::guide_legend(override.aes = list(linewidth = 5))) + #rescale segment in legend
     theme_minimal()+
     ggplot2::ggtitle(title) +
ggplot2::theme(
  plot.title = ggplot2::element_text(hjust = 0.5, margin = ggplot2::margin(b = 10)),
  plot.margin = ggplot2::margin(t = 40, r = 10, b = 40, l = 10),
  axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
  axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
  axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)))+
coord_flip()+
   ggplot2::scale_x_reverse()
   
   print(p)
 }
 
 else{
   dirtdata$profile_fill <- factor(profileName) #use profileName argument to add a label on y
    
  p<- ggplot2::ggplot(dirtdata, ggplot2::aes(x=starts,profile_fill,color = factor(cluster))) +#initiate ggplot
  ggplot2::scale_x_continuous(limits = c(minDepth, maxDepth),name = "Depth (cm)")+ #set up x-axis
  ggplot2::scale_y_discrete(limits = levels(dirtdata$profile_fill))+
  ggplot2::geom_segment(ggplot2::aes(x = starts, y = profile_fill, xend = ends, yend = profile_fill, colour = factor(cluster)), data = dirtdata, linewidth = 5)+#segment drawing
  ggplot2::scale_color_manual(values = clusterColors, aesthetics = c("color"),name = "Cluster")+#make colors what you want
  ggplot2::guides(size = "none", color = ggplot2::guide_legend(override.aes = list(linewidth = 10))) + #rescale segment in legend
    theme_minimal()+
    ggplot2::ggtitle(title) +
ggplot2::theme(
  plot.title = ggplot2::element_text(hjust = 0.5, margin = ggplot2::margin(b = 10)),
  plot.margin = ggplot2::margin(t = 40, r = 10, b = 40, l = 10),
  axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
  axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
  axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)))+
ggplot2::coord_flip()+
    ggplot2::scale_x_reverse()
    
  
  print(p)
 }
  
  
}}


#Examples 
testPit3 <- testPit

testPit3 <- select(testPit3, c(-Munsell, -SampleID))

clastCluster(testPit3, distance = "E",depth = "Depth_cm",profile = "ProfileID",PCA = TRUE,plot = TRUE,maxDepth=100)

clastCluster(testPit3, distance = "G",depth = "Depth_cm",profile = "ProfileID",PCA =TRUE,plot = TRUE, minK = 2, maxK=8, title="Cluster Analysis of Arroyo Profiles")
