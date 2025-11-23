loamRanger <- function(dirtdata, depth, sand, silt, sandcolor, siltcolor, claycolor, depth_intervals = 10, mindepth=0, maxdepth=NA, title=NA){
  texture_df <- data.frame(
    depth = dirtdata[[depth]],
    sand  = dirtdata[[sand]],
    silt  = dirtdata[[silt]]
  )
  
  #make sure its numeric
  texture_df$depth <- as.numeric(texture_df$depth)
   texture_df$sand <- as.numeric(texture_df$sand)
    texture_df$silt <- as.numeric(texture_df$silt)
  
  #add clay as remainder
  texture_df$clay <- 100 - texture_df$sand - texture_df$silt

  #need to pivot longer for bars to stack 
  texture_long <- tidyr::pivot_longer(
  texture_df,
  cols = c("sand", "silt", "clay"),
  names_to = "component",
  values_to = "percent"
)
#setting up for tick marks
if (is.na(maxdepth)){
  maxdepth= max(texture_long$depth, na.rm = TRUE)
}

depthticks <- seq(
  from = mindepth,
  to   = maxdepth,
  by   = depth_intervals
)
  
 ggplot2::ggplot(texture_long, ggplot2::aes(x = depth, y = percent, fill = component)) + #colors correctly
  ggplot2::geom_col(alpha = 0.7) +
  ggplot2::scale_fill_manual(values = c(
    sand = sandcolor,
    silt = siltcolor,
    clay = claycolor
  )) +
  ggplot2::coord_flip() + 
   ggplot2::scale_x_reverse(breaks = depthticks) + #so zero is at top
  ggplot2::labs(x = "Depth", y = "Percent", fill = "")+
   ggplot2::theme_minimal() +
   ggplot2::ggtitle(title) +
ggplot2::theme(
  plot.title = ggplot2::element_text(hjust = 0.5, margin = ggplot2::margin(b = 10)),
  plot.margin = ggplot2::margin(t = 40, r = 10, b = 40, l = 10),
  axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
  axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
  axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20))
)
}



#example
testPit2 <- filter(testPit, ProfileID=="Profile_1")
loamRanger(testPit2, depth = "Depth_cm", sand = "Sand_pct", silt = "Silt_pct", sandcolor = "blue",siltcolor = "green", claycolor = "purple", mindepth=0, title = "Texture")





