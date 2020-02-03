#' Prepare the phenotypic data
#'
#' This function prepares the phenotypic data of the subjects. It inputs the phenotypic information and the column numbers of scan ID (colID), subject ID (colsID), scan timepoint (colTp) and age (colAge). It reorganises the information, to output a data frame which has the information needed to calculate the APCs for each subject and its two timepoints (scan ID and age at first and second timepoint).
#'
#' @param file a data frame with the phenotypic information of subjects.
#' @param colID a number defining the column where the scan ID is specified.
#' @param colsID a number defining the column where the subject ID is specified.
#' @param colTP a number defining the column where the timepoint is specified. All subjects must have timepoint 1.
#' @param colAge a number defining the column where the age of the patient at the scan is specified
#'
#' @return Data frame
#'
#' @examples
#' y <- load_phenotypes(x, colID = 1, colsID = 2, colTp = 3, colAge = 4)
#'
#' @author Lucia de Hoyos <luciadeh@gmail.com>
#'
#' @export
load_phenotypes <- function(file, colID= NULL, colsID= NULL, colTp= NULL, colAge= NULL){
  phenotypes <- data.frame(ID= file[,colID], subID= file[,colsID], timepoint = file[,colTp], age= file[,colAge], stringsAsFactors = F)
  tp1 <- subset(phenotypes, timepoint == 1); colnames(tp1) <- c("ID1", "subID", "tp1", "age1")
  tp_no <- subset(phenotypes, timepoint != 1); tp_no <- tp_no[order(tp_no$timepoint),]

  tp2 <- c()
  for (i in 1:nrow(tp1)){
    tp2 <- rbind(tp2, tp_no[match(tp1$subID[i], tp_no$subID),])
  }
  colnames(tp2) <- c("ID2", "subID", "tp2", "age2")
  tp <- merge(tp1, tp2)

  return(tp)
}

