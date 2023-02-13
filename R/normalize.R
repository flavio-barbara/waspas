#' @title normalize
#'
#' @description Normalize the values of Alternatives X Criteria matrix according to a given
#'              Cost-Benefit vector of flags
#' @param values A data set object with Alternatives X Criteria  values to be normalized
#' @param flags A vector of flags that determines if the criterion is a Cost or Benefit one
#'              Must be same size of Criteria, must contains just strings initiated with B or C
#' @param initCol First Column with values of input matrix (to avoid metadata), Default = 1
#' @param initRow First Row with values of input matrix (to avoid metadata), Default = 1
#'
#' @return A data frame object that contains the input matrix with its values normalized
#'
#' @examples
#' normalize(dfMatrix, vCost_Benefit, initCol, initRow)
#' normalized_matrix <- normalize(row_values_matrix, flags_Cost_Benefit, initCol = 2, initRow = 4)
#' @export
#' @importFrom readxl "%>%"


#' #################### Normalization: dfMatrix Matrix  ==>  AxCNorm Matrix

normalize <- function(dfMatrix, vCost_Benefit, initRow = 1, initCol = 1) {
  # Test vector of flags X matrix of values dimentions
  workingMatrix <- dfMatrix[initRow:nrow(dfMatrix), initCol:ncol(dfMatrix)]
  if (length(vCost_Benefit) != ncol(workingMatrix)) {
    return("Error #01: Vector of Cost-Benefit flags must be same size of number of Criteria")
  }
  # Test flags contents, just strings initiated with B (Benefit) ou C (Cost) are permitted
  justBorC <- sort(unique(toupper(substr(vCost_Benefit,1,1))))
  if (!identical(justBorC, c("B","C"))) {
    return("Error #02: Vector of flags must contains just strings initiated with B or C (i.e. b,c,B,C,Cost,Benefit,Ben etc.)")
  }
  # Normalization loop
  flagsCxB <- toupper(substr(vCost_Benefit,1,1))
  for(iCol in 1:ncol(workingMatrix)){
    vAlternativeValues <- workingMatrix[1:nrow(workingMatrix),iCol]
    vAlternativeValues <- sapply(vAlternativeValues, as.numeric)
    maxv <- max(vAlternativeValues)
    minv <- min(vAlternativeValues)
    for(iRow in initRow:nrow(workingMatrix)){
      if (flagsCxB[iCol] == "C"){
        workingMatrix[iRow,iCol] <- toString(minv / as.numeric(workingMatrix[iRow,iCol]))
      } else {  #"Cost-Benefit"] == "B" (Benefit)
        workingMatrix[iRow,iCol] <- toString(as.numeric(workingMatrix[iRow,iCol]) / maxv)
      }
    }}
  return(workingMatrix)
}
