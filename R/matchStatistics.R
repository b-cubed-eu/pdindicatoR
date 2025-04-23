#' Matching statistics
#'
#' This function takes the results obtained from the taxonmatchGBIF function and
#' provides summary statistics on the different types of matches that were obtained
#' from it. This function is designed to evaluate data quality
#' @param matches a dataframe obtained from the taxonmatchGBIF function.
#' @returns prints a summary for each match type in the terminal
#' @examples
#' \dontrun{ex_data <- retrieve_example_data()
#' mtable <- taxonmatch(ex_data$tree, nmbCores=4)
#' matchStatistics(mtable)}
#' @export
#'

matchStatistics <- function(matches){
  #Extract the indices corresponding to each matching type
  exactIdx<-which(matches$matchType=="EXACT")
  fuzzyIdx<-which(matches$matchType=="FUZZY")
  higherrankIdx<-which(matches$matchType=="HIGHERRANK")
  noneIdx<-which(matches$matchType=="NONE")

  #Compute the amount of each matchtype
  nmbExact<-length(exactIdx)
  nmbFuzzy<-length(fuzzyIdx)
  nmbHigherRank<-length(higherrankIdx)
  nmbNone<-length(noneIdx)
  nmbTips<-nrow(matches)

  #Return the total numbers of each match type and the fraction
  print(paste("The number of exact matches:", nmbExact, "(", (nmbExact / nmbTips) * 100, "%)"))
  print(paste("The number of fuzzy matches:", nmbFuzzy, "(", (nmbFuzzy / nmbTips) * 100, "%)"))
  print(paste("The number of higher rank matches:", nmbHigherRank, "(", (nmbHigherRank / nmbTips) * 100, "%)"))
  print(paste("The number of none matches:", nmbNone, "(", (nmbNone / nmbTips) * 100, "%)"))
}
