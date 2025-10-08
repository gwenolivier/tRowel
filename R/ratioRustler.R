#' Calculate Element Ratios
#' @param df A data frame 
#' @param numerator Vector or list of vectors. Column(s) in the numerator (if user selects multiple columns, the function takes the sum of all columns). If provided as a list, multiple ratios can be calculated.
#' @param denominator Vector or list of vectors. Column(s) in the denominator (if user selects multiple columns, the function takes the sum of all columns). If provided as a list, multiple ratios can be calculated. If the denominator value is zero, it'll place an NA
#' @param new_col Character. Option to add a name for the new ratio column. If NULL, the function will generate a name. If lists are provided for ratio calculations, new_col must be the same length. Default is NULL (i.e. function generates names for you).
#' @param removeNAs Logical - defaults to TRUE (meaning it will remove any NAs from the selected columns)
#' @return A copy of your data frame with the new ratio column. Save the function to a data frame to save the data in your environment (i.e., df_with_ratio <- ratioRustler(df, numerator, denominator))
#' @author Gwen Olivier, Samuel R. Borstein
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @examples
#' #Example with list input
#' nums <- list(c("Fe","Ca"),"Al", "Si")
#' denoms <- list("K","P",c("Sr","Fe"))
#' data("testPit")
#' ratioRustler(df = testPit, numerator = nums, denominator = denoms, new_col = NULL, 
#' removeNAs = TRUE)
#' 
#' #Example with vector input while specifying name of new column
#' ratioRustler(df = testPit, numerator = "Fe",denominator = "Ca", new_col = "MyRatio", 
#' removeNAs = TRUE)
#' 
#' @export


ratioRustler <- function(df, numerator, denominator, new_col = NULL, removeNAs = TRUE){
  if(is.list(numerator)&&is.list(denominator)){#if list input, start loop
    
    #kill function if numerator length and denominator length differ
    if(!length(numerator)==length(denominator)){
      stop("Length of lists of numerators and denominators differs.")
    }
    
    #kill 
    if(!is.null(new_col)){
      if(!length(numerator)==length(new_col)){
        stop("Length of lists of numerators/denominators differs from length of names for columns to be named.")
      }
    }
    
    for(ratio.index in 1:length(numerator)){
      if (is.null(new_col)) {
        new_col_name <- paste0(
          paste(numerator[[ratio.index]][], collapse = "+"),
          "_over_",
          paste(denominator[[ratio.index]][], collapse = "+"),
          "_ratio"
        )
      }else{
        new_col_name <- new_col[ratio.index]
      }
      df <- dplyr::mutate(
        .data = df,
        temp_num = rowSums(dplyr::pick(numerator[[ratio.index]][]), na.rm = removeNAs), #sums the col(s) picked for num
        temp_den = rowSums(dplyr::pick(denominator[[ratio.index]][]), na.rm = removeNAs), #sums the col(s) picked for den
        !!rlang::sym(new_col_name) := temp_num / dplyr::na_if(temp_den, 0) #!!rlang needed so new_col reflects the name assigned above, puts in NA if denominator is 0
      ) |>
        dplyr::select(-temp_num, -temp_den) #deletes temp columns
    }
  }else{
    if (is.null(new_col)) {
      new_col <- paste0(
        paste(numerator, collapse = "+"),
        "_over_",
        paste(denominator, collapse = "+"),
        "_ratio"
      )
    }
    df <- dplyr::mutate(
      df,
      temp_num = rowSums(dplyr::pick({{ numerator }}), na.rm = removeNAs), #sums the col(s) picked for num
      temp_den = rowSums(dplyr::pick({{ denominator }}), na.rm = removeNAs), #sums the col(s) picked for den
      !!rlang::sym(new_col) := temp_num / dplyr::na_if(temp_den, 0) #!!rlang needed so new_col reflects the name assigned above, puts in NA if denominator is 0
    ) |>
      dplyr::select(-temp_num, -temp_den) #deletes temp columns
  }
  return(df)
}

