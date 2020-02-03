#' Calculate the annual porcentual change
#'
#' This function calculates the annual porcentual change (APC) of a matrix which has the subject ID as its first column and on the rest of the columns the numeric data to calculate the APC from. Therefore, each row is a different scan ID and each column a different region.
#'
#' @param fdata Data frame which has on its first column the subject ID and on the rest of the columns the data to calculate the APC from.
#' @param f_ph Data frame, output from function load_phenotypes().
#'
#' @return Data frame
#'
#' @author Lucia de Hoyos <luciadeh@gmail.com>
#'
#'
#' @export
#'

calcAPC <- function(fdata, f_ph){
  nsubs <- nrow(f_ph)
  nregs <- ncol(fdata) - 1
  colnames(f_ph) <- c("subID", "ID1", "tp1", "age1", "ID2", "tp2", "age2")
  colnames(fdata)[1] <- "ID"

  mat <- matrix(data= 0, nrow= nsubs, ncol= nregs)

  # Calculate APC
  for (j in 1:nsubs){
    d1 <- fdata[match(f_ph$ID1[j], fdata$ID),]; d1 <- as.numeric(d1[,-1])
    d2 <- fdata[match(f_ph$ID2[j], fdata$ID),]; d2 <- as.numeric(d2[,-1])
    agediff <- as.numeric(f_ph$age2[j] - f_ph$age1[j])

    mat[j,] <- 100*((d2-d1)/((d2+d1)/2))/agediff
  }

  mat <- data.frame(subID= f_ph$subID, mat, stringsAsFactors = F)
  colnames(mat) <- c("subID", colnames(fdata)[2:ncol(fdata)])

  return(mat)
}

