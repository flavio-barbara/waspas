#' @title calcWSM
#'
#' @description Calculates the ranking for the alternative's set according to WSM method.
#' @param AxCNorm A data set object with normalized values of Alternatives X Criteria
#' @param vWeights Contains a set of user-assigned values to weight the criteria.
#'                 The sum of these weights must add up to 1.
#'                 The format of this input is an array of values.
#'
#' @return A data frame object that contains 2 columns and the the same number of rows as
#'        the input matrix. The columns "Points" has the calculated relative value of each
#'        alternative whose id is in the "Alternatives" column.
#'
#' @examples
#' calcWSM(normalized_matrix, vector_weights)
#' wsm_matrix <- calcWSM(row_values_matrix, vector_weights)
#'
#' @export

#################### Ranking for WSM Method: AxCNorm Matrix  ==>  AxC_WSM Matrix
calcWSM <- function(AxCNorm, vWeights) {
  # Test vector of Weights X matrix of values dimentions
  workingMatrix <- AxCNorm
  if (length(vWeights) != ncol(workingMatrix)) {
    return("Error #03: Vector of Weights values must be same size of number of Criteria")
  }
  # Test Vector of Weights contents, it must summarize 1
  if (sum(sapply(vWeights,as.numeric)) != 1) {
    return("Error #04: Values in Vector of Weights must summarize 1!")
  }
  # WSM Calculation loop
  Points = rep(0,nrow(workingMatrix))
  Alternatives <- 1:nrow(workingMatrix)

  browser()

  AxC_WSM <- cbind(Alternatives, Points)
  for(iCol in 1:ncol(workingMatrix)){
    for(iRow in 1:nrow(workingMatrix)){
      workingMatrix[iRow,iCol] <- toString(as.numeric(workingMatrix[iRow,iCol])
                                           * as.numeric(vWeights[iCol]))
    }}
  # calculate ranking
  for(iRow in 1:nrow(workingMatrix)){
    # v <- AxC_WSM[iRow, 1:ncol(AxC_WSM)-1]
    # v <- sapply(v,as.numeric)
    AxC_WSM[iRow,"Points"] <- sum(sapply(workingMatrix[iRow,],as.numeric))
  }
  vWSM <- AxC_WSM[,c("Alternatives","Points")]
  return(vWSM)
}
