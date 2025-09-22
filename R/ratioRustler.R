#' @param df A data frame 
#' @param numerator Column(s) in the numerator (if user selects multiple columns, the function takes the sum of all columns)
#' @param denominator Column(s) in the denominator (if user selects multiple columns, the function takes the sum of all columns). If the denominator value is zero, it'll place an NA
#' @param new_col Option to add a name for the new ratio column. If NULL, the function will generate a name
#' @param removeNAs Logical - defaults to TRUE (meaning it will remove any NAs from the selected columns)
#' @return A copy of your data frame with the new ratio column. Save the function to a data frame to save the data in your environment (i.e., df_with_ratio <- ratioRustler(df, numerator, denominator))


ratioRustler <- function(df, numerator, denominator, new_col = NULL, removeNAs = TRUE){
  if (is.null(new_col)) {
    new_col <- paste0(
      paste(numerator, collapse = "+"),
      "_over_",
      paste(denominator, collapse = "+"),
      "_ratio"
    )
  }
  
  dplyr::mutate(
    df,
    temp_num = rowSums(dplyr::pick({{ numerator }}), na.rm = removeNAs), #sums the col(s) picked for num
    temp_den = rowSums(dplyr::pick({{ denominator }}), na.rm = removeNAs), #sums the col(s) picked for den
    !!rlang::sym(new_col) := temp_num / dplyr::na_if(temp_den, 0) #!!rland needed so new_col reflects the name assigned above, puts in NA if denominator is 0
  ) |>
    dplyr::select(-temp_num, -temp_den) #deletes temp columns
}
