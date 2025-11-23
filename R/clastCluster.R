#list of items to cover on 11/24 meeting
  #finishing gowers part of clastCluster
  #updating testPit to have elevations
  #finalizing loamRanger texture function 


library(factoextra)
library(gawdis)
library(tidyverse)
library(magrittr)





clastCluster <- function(dirtdata, distance, depth, profile=NULL, plot=TRUE, elevation=NULL, PCA=FALSE, nstart=50){
  
#prep data
  dirtdata[[depth]] <- as.factor(dirtdata[[depth]]) #need this as a factor to plot later
  
  if (!is.null(profile)){
   dirtdata[[profile]] <- as.numeric(dirtdata[[depth]]) #make all the same data type, I think numeric is correct? Not sure if it matters for the plot later
   dirtdata2 <- dirtdata  %>% select(-profile)
  }
  
  dirtdata2 <- dirtdata %>% select(-depth, -elevation) #get rid of cols
  
  
  
#Cluster Analysis part of function
  if(distance == "E"){
    dirtdata2 %<>% mutate_at(scale)
    silPlot <- fviz_nbclust(dirtdata2, kmeans, method = "silhouette") #cluster reccomendation 

  print(silPlot) 
  
  #ask user to input number of clusters
  k <- readline(prompt = "Type the number of clusters for your analysis and hit enter")

  #convert to numeric or else it'll be read as a string
  k <- as.numeric(k)
  
  km.res <- kmeans(pxrf_w, k, nstart = nstart) 
  
  if(PCA==TRUE){
  
 pcaPlot <- fviz_cluster(km.res, data = dirtdata2, geom="point") + geom_text(aes(label=dirtdata$depth))
 print(pcaPlot)
  }
  }
  
if(distance == "G"){    #not sure what ord would be best for soil, do you usually keep opti min/max to their default?
  gawdis(dirtdata2, ord= , w.type=equal, )
  
}
  
 #I am a little lost on what exactly the output looks like (looking at the vignette), do you have some examples from your own work? I could be confusing myself because I am expecting something similar to k-means
  
}
