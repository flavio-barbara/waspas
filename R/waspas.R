#' @title waspas
#'
#' @description Apply the lambda to assign a relative importance to each of the previously
#' used methods (WSM and WPM). Lambda values range from zero to one.
#' @param WSM_matrix The data set object obtained from the application of the WSM method (calcWSM function)
#' @param WPM_matrix The data set object obtained from the application of the WPM method (calcWPM function)
#' @param lambda The lambda value (between 0 and 1)
#'
#' @return A data frame object that contains the alternatives set scored by and classified in descending
#' order (from best to worst classified) according to the weighting proposed by the WASPAS method
#' using the input lambda.
#'
#' @examples
#' waspas(WSM_matrix, WPM_matrix, lambda)
#' waspas_rank <- waspas(WSM_matrix, WPM_matrix, lambda)
#' waspas_rank <- waspas(WSM_matrix, WPM_matrix, 0)
#' waspas_rank <- waspas(WSM_matrix, WPM_matrix, 0.5)
#' waspas_rank <- waspas(WSM_matrix, WPM_matrix, 0.99)
#' @export


#' #################### Normalization: dfMatrix Matrix  ==>  AxCNorm Matrix

waspas <- function(WSM_matrix, WPM_matrix, lambda) { browser()
  # Test value of lambda
  if (! (as.numeric(lambda) >= 0 & as.numeric(lambda) <= 1)) {
    return("Error #07: The lambda's value must be between 0 and 1")
  }
  # Test WSM_matrix X WPM_matrix (size and contents)
  if (nrow(WSM_matrix) != nrow(WPM_matrix)) {
    return("Error #08: WSM and WPM matrices entered must have same number of rows")
  }else{
    # if (nrow(WSM_matrix) != nrow(alternatives)) {
    #   return("Error #09: The list of alternatives must have the same number of rows as the WSM and WPM matrices")
    # }else{
    for (iRow in 1:nrow(WSM_matrix))
    if (! identical(as.vector(WSM_matrix[,1]), as.vector(WPM_matrix[,1]))) {
      return("Error #10: Both matrices (WSM and WPM) entered must have identical Alternatives")
  }}
  # WASPAS Ranking
  waspas_matrix <- cbind(WSM_matrix,WPM_matrix[,"Points"], WASPAS=0.0)
  colnames(waspas_matrix) <- c("Alternative", "WSM_Rank","WPM_Rank","WASPAS_Rank")
  waspas_matrix[,"WASPAS_Rank"] <- waspas_matrix[,"WSM_Rank"] * lambda + waspas_matrix[,"WPM_Rank"] * (1-lambda)
  browser()
  return(as.data.frame(waspas_matrix))
}
