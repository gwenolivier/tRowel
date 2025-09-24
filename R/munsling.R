#' @param soil_df A dataframe with a column that has Munsell colors (i.e., 10YR 2/1) 
#' @param munsell_column The column name with Munsell colors (must be a character and properly formatted (i.e., 10YR 2/1 NOT 10YR2/1)
#' @return This function will return the original dataframe with new L, a, b columns
#' @examples MunsellClusterData <- munsling(testPit, munsell_column = "Munsell")  
#' @details This function saves the columns to your original dataframe stored in R. Your new df will look the exact same with three new columns (L, a, b)
#' @author Gwen Olivier, Sam R Borstein
#' @export

munsling <- function(soil_df, munsell_column){
  LAB <- munsellinterpol::MunsellToLab(soil_df[[munsell_column]])
  new_df <- cbind(soil_df, LAB)
  

  
  return(new_df)
  
  
} 
