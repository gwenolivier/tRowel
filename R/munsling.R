#' Converts a Munsell specification to CIE Lab coordinates for clustering
#' @param dirtData A dataframe with a column that has Munsell colors (i.e., 10YR 2/1) 
#' @param munsellCol The column name with Munsell colors (must be a character and properly formatted (i.e., 10YR 2/1 NOT 10YR2/1)
#' @return This function will return the original dataframe with new L, a, b columns
#' @examples MunsellClusterData <- munsling(testPit, munsellCol = "Munsell")  
#' @details This function saves the columns to your original dataframe stored in R. Your new df will look the exact same with three new columns (L, a, b)
#' @author Gwen Olivier, Samuel R. Borstein
#' @export
#check Munsell white param in munsellinterpol 

munsling <- function(dirtData, munsellCol, whiteVal='D65'){
  LAB <- munsellinterpol::MunsellToLab(dirtData[[munsellCol]],white=whiteVal)
  df_LAB <- cbind(dirtData, LAB)
  

  
  return(df_LAB)
  
  
} 
