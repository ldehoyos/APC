#' Compute and compare the structural covariance matrices & output the text files.
#'
#' This function creates the structural covariance (SC) matrices from the two input datasets (data1, data2) and then compares them using Z-scores. The function then outputs a matrix with p-values that indicate which differences between the two matrices are significant. To do the comparison, the function substracts data1 to data2.
#'
#' @param data1 Data frame 1 with information to be converted into a SC matrix. Do not input subject ID, only metrics values.
#' @param data2 Data frame 2 with information to be converted into a SC matrix. Do not input subject ID, only metrics values.
#' @param fname1 Character string with the name of the file to save the SC matrix of data1.
#' @param fname2 Character string with the name of the file to save the SC matrix of data2.
#'
#' @return Matrix of p-values.
#'
#' @author Lucia de Hoyos <luciadeh@gmail.com>
#'
#'
#' @export
#'


runSCwr <- function(data1, data2, fname1, fname2){
  require(DescTools)
  # Compute SC matrices
  mat1 <- cor(data1)
  mat2 <- cor(data2)

  write.table(mat1, file = fname1, row.names = T, col.names = T)
  write.table(mat1, file = fname2, row.names = T, col.names = T)

  z1 <- FisherZ(mat1)
  z2 <- FisherZ(mat2)
  diffmat <- z1^2- z2^2
  diffmat <- as.matrix(diffmat)

  # Fisher test on diff matrix
  test <- diffmat/(sqrt(1/(nrow(data1)-3)+1/(nrow(data2)-3)))

  # Set NaN to 0
  test[is.nan(test)] = 0
  # Get P-values
  pmat = 2*pnorm(-abs(test))
  # Replace nan with 0's.
  pmat[!is.finite(pmat)] <- 0

  ## FDR correction
  # Lower triangle
  ltri <- lower.tri(pmat)
  pmat[ltri] <- p.adjust(pmat[ltri], method = "fdr")
  # Upper triangle
  utri <- upper.tri(pmat)
  pmat[utri] <- t(pmat)[utri]

  # Save the matrix
  pmat <- data.frame(pmat); colnames(pmat) <- c()

  return(pmat)
}
