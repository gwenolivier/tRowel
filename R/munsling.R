#' @param soil_df A dataframe with a column that has Munsell colors (i.e., 10YR 2/1) 
#' @param munsell_column The column name with Munsell colors (must be a character and properly formatted (i.e., 10YR 2/1 NOT 10YR2/1)
#' @return This function will return the original dataframe with new L, A, B columns
#' @export
#' @examples MunsellClusterData <- munsling(testPit, munsell_column = "munsell")  

munsling <- function(soil_df, munsell_column){
  LAB <- munsellinterpol::MunsellToLab(soil_df[[munsell_column]])
  new_df <- cbind(soil_df, LAB)
  
  print("Remember to save this function to a dataframe (i.e., data_with_munsell <- munsling(...)). Now you can use these three columns in one of the tRowel cluster functions - use all three columns (L, a, & b) so it clusters accurately")
  
  return(new_df)
  
  
} 
