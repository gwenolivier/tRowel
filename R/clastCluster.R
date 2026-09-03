#' Statistically cluster (Gower's or Euclidean) and graphically display soil/sediment data as a profile
#' @param dirtdata Data frame containing soil characteristic data for clustering.Should have unique rownames.
#' @param distance Character. Either Euclidean or Gower. If you have only numeric data, use Euclidean. If your data contains a mix of data types, use Gower.
#' @param depthStart Character. Name of the column in dirtdata containing information on the starting depth of sample.
#' @param depthEnd Character. Name of the column in dirtdata containing information on the ending depth of sample.
#' @param profile Character. Name of column containing profile IDs if multiple soil profiles exist in dirtdata. If a single profile, set to Null. Default is NULL (i.e. a single profile).
#' @param ordination Logical. Should an ordination of the data be performed and plotted. Default is FALSE. If distance = Euclidean, Principle Component Analysis (PCA) is performed. If distance = Gower, a Principle Co-ordinate Analysis (PCoA) is performed.
#' @param minK Numeric. Minimum number of clusters to try. Default is 2.
#' @param maxK Numeric. Maximum number of clusters to try. Must be less than the number of rows in dirtdata. Default is 10.
#' @param nstart Numeric. Number of random starts to be used in clustering algorithm.
#' @param plot Logical. Should profile cluster plots be generated. Default is TRUE. 
#' @param depthticks Numeric. Interval for plotting depth. Default is 10.
#' @param minDepth Numeric. Minimum depth to be used in plotting. Default is 0.
#' @param maxDepth Numeric. Maximum depth to be used in plotting. Default is NULL.
#' @param clusterColors Character. Colors to be used in plotting. Default is random soil-like colors.
#' @param profileOrder Character. Order in which to plot profiles if multiple soil profiles are present in dirtdata.
#' @param profileName Character. If a single profile. What should profile be named? Default is Null.
#' @param plotTitle Character. Title for ordination plots. Default is NULL.
#' @param labelOrientation Numeric. Controls angle of orientation of axis labels. 
#' @param scaleData Logical. For Euclidean, should data be scaled prior to clustering.
#' @param weight Character. Type of weighting to be used if distance is Gower. See options in gawdis::gawdis. Default is equal to handle NA values.
#' @param groups Numeric vector. Vector of trait groupings that are considered to represent related soil information (i.e. L, a, b). By default each trait is treated separately (groups = NULL). In order to define groups use the same values, e.g. groups = c(1,2,2,2,3,3) in case of 6 variables attributed to 3 groups, with the length of vector that should be the same as the number of variables in the dataset.
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @return A list Containing the data and plots.
#' @examplesIf interactive()
#' \donttest{
#' data("testPit")
#' #Prep data
#' testPit_withMun <- munsling(testPit, munsellCol = "Munsell")
#' testPitM <- testPit_withMun
#' testPitM <- select(testPitM, -c(Munsell) )
#' #Example using Euclidean distance and PCA
#' EuclidExample <- clastCluster(dirtdata = testPitM, distance = "E", depthStart = "Depth_cm_Start",
#'  depthEnd = "Depth_cm_End", profile = "ProfileID", ordination = TRUE, plot = TRUE, maxDepth = 150,
#'   depthticks = 10)
#' 
#' #Example using Gower's distance and PCoA
#' GowerExample <- clastCluster(dirtdata = testPitM, distance = "G",depthStart = "Depth_cm_Start", 
#'   depthEnd = "Depth_cm_End", minK = 2, maxK=8, profile = "ProfileID", ordination = TRUE, 
#'   plot = TRUE, plotTitle="Cluster Analysis of Arroyo Profiles", maxDepth = 150, depthticks = 10)
#' }
#' @export
#' @author Gwen Olivier, Samuel R. Borstein

clastCluster <- function(dirtdata, distance, depthStart, depthEnd, profile=NULL, ordination=FALSE, minK=2, maxK=10, nstart=50, plot=TRUE, depthticks=10, minDepth=0, maxDepth=NULL, 
                         clusterColors = 
                           c("#DCC7AA",
                             "#A68A6D", 
                             "#6E5A48", 
                             "#D29F6C",
                             "#E8D3A1", 
                             "#4F4336",
                             "#AAB8B4",
                             "#4A615D",
                             "#B3A79E",
                             "#8B3A1C",
                             "#C15629"), profileOrder=NULL, profileName = NULL, plotTitle=NULL, labelOrientation = 45, scaleData = TRUE, weight = "equal", groups = NULL){
  
  #prep data
  #dirtdata[[depth]] <- as.factor(dirtdata[[depth]]) #need this as a factor to plot later
  
  dirtdata2 <- dplyr::select(dirtdata, -dplyr::all_of(c(depthStart, depthEnd)))
  
  if (!is.null(profile)) {
    dirtdata2 <- dplyr::select(dirtdata2, -dplyr::all_of(profile))
  }
  
  
  #Cluster Analysis part of function
  if(distance %in% c("E","e","Euclidean","euclidean")){
    if(scaleData == TRUE){
      dirtdata2 <- dirtdata2 %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), scale))
    }
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
      #pca_df$depth <- dirtdata[[depth]]
      pcaPlot2 <- ggplot2::ggplot(pca_df, ggplot2::aes(x = .data$x, y = .data$y, color = .data$cluster)) +
        ggplot2::geom_point()
    }
    dirtdata$cluster <- km.res$cluster
  }
  
  if(distance %in% c("G","Gower","g","gower")){ 
    
    #calc Gower's distance
    MyDist <- gawdis::gawdis(dirtdata2, w.type = weight, groups = groups)
    
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
                                k = K_Range[i], nstart = nstart)
        
        sil_width[i] <- pam_fit$silinfo$avg.width
        names(sil_width) <- paste0("K_",K_Range)
        
      }
      if(PlotSil == TRUE){
        Best <- which(sil_width==max(sil_width))
        Best <- as.numeric(gsub("K_","",names(Best)))
        Avg_Sil <- ggplot2::ggplot(data = cbind.data.frame(K_Range, sil_width),  mapping = ggplot2::aes(x = K_Range, y = sil_width))+
          ggplot2::geom_line(linewidth = 1)+
          ggplot2::geom_point()+
          ggplot2::geom_vline(xintercept = Best, linewidth = 1, linetype = "dashed")+
          ggplot2::labs(
            x = "Number of cluster K",
            y = "Average silhouette width",
            title = "Optimal number of clusters (method = \"silhouette\")"
          )+
          ggplot2::theme_classic()
        print(Avg_Sil)
      }
      return(Avg_Sil)
    }
    #Average silhouette 
    Avg_Sil <- MineClusters(distance = MyDist,GminK = minK, GmaxK = maxK, PlotSil = TRUE, col = "black")
    
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
      ggplot2::labs(title = plotTitle, x = paste0("PC1 (",round(res$values$Rel_corr_eig[1],4)*100,"%)"),
                    y = paste0("PC2 (",round(res$values$Rel_corr_eig[2],4)*100,"%)"))
  }
  
  #Plotting time
  if (plot==TRUE){
    
    dirtdata <- dirtdata %>% 
      dplyr::mutate(starts = .data[[depthStart]],
                    ends   = .data[[depthEnd]])
    
    dirtdata <- dplyr::select(dirtdata, -dplyr::all_of(c(depthStart, depthEnd)))
    
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
        ggplot2::scale_x_reverse(limits = c(minDepth, maxDepth),name = "Depth (cm)", breaks = seq(minDepth, maxDepth, by = depthticks))+ #set up x-axis
        ggplot2::scale_y_discrete(limits = levels(dirtdata[[profile]]))+
        ggplot2::geom_segment(ggplot2::aes(x = .data$starts, y = .data[[profile]], xend = .data$ends, yend = .data[[profile]], colour = factor(.data$cluster)), data = dirtdata, linewidth = 5)+#segment drawing
        ggplot2::scale_color_manual(values = clusterColors, aesthetics = c("color"),name = "Cluster")+#make colors what you want
        ggplot2::guides(size = "none", color = ggplot2::guide_legend(override.aes = list(linewidth = 5))) + #rescale segment in legend
        ggplot2::theme_minimal()+
        ggplot2::ggtitle(plotTitle) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, margin = ggplot2::margin(b = 10)),
          plot.margin = ggplot2::margin(t = 40, r = 10, b = 40, l = 10),
          axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
          axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
          axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)),
          axis.text.x = element_text(angle = labelOrientation, hjust = 1)
          )+
        ggplot2::coord_flip()
      
      
    }else{
      dirtdata$profile_fill <- factor(profileName) #use profileName argument to add a label on y
      
      p<- ggplot2::ggplot(dirtdata, ggplot2::aes(x=.data$starts, .data$profile_fill, color = factor(.data$cluster))) +#initiate ggplot
        ggplot2::scale_x_reverse(limits = c(minDepth, maxDepth),name = "Depth (cm)", breaks = seq(minDepth, maxDepth, by = depthticks))+ #set up x-axis
        ggplot2::scale_y_discrete(limits = levels(dirtdata$profile_fill))+
        ggplot2::geom_segment(ggplot2::aes(x = .data$starts, y = .data$profile_fill, xend = .data$ends, yend = .data$profile_fill, colour = factor(.data$cluster)), data = dirtdata, linewidth = 5)+#segment drawing
        ggplot2::scale_color_manual(values = clusterColors, aesthetics = c("color"),name = "Cluster")+#make colors what you want
        ggplot2::guides(size = "none", color = ggplot2::guide_legend(override.aes = list(linewidth = 10))) + #rescale segment in legend
        ggplot2::theme_minimal()+
        ggplot2::ggtitle(plotTitle) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, margin = ggplot2::margin(b = 10)),
          plot.margin = ggplot2::margin(t = 40, r = 10, b = 40, l = 10),
          axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
          axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
          axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)),
          axis.text.x = element_text(angle = labelOrientation, hjust = 1)
          )+
        ggplot2::coord_flip()
      
      
      
    }
    
    plots <- list(
      silhouette = Avg_Sil, 
      main = p
    )
    
    if (ordination) {
      plots$ordination <- pcaPlot
    } 
    clastCluster.Res <- list(data  = dirtdata, plots = plots)
    class(clastCluster.Res) <- "classCluster"
    return(clastCluster.Res)
  }
}
