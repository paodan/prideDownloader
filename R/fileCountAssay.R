#' The number of files in the assay
#' @param assayAccession character, a vector of project accessions.
#' The default is c("22134", "22135")
#' @return a data.frame, the number of files in the assay.
#' @import RCurl
#' @import RJSONIO
#' @export
#' @examples {
#' \dontrun{
#' fileCountAssay("22135")
#' fileCountAssay(c("22134", "22135"))
#' }
#' }
fileCountAssay = function(assayAccession = c("22134", "22135"),
                            accessionRownames = TRUE){

  if (length(assayAccession) < 1){
    stop("Must provide at least one assayAccession!")
  }

  baseURL = "https://www.ebi.ac.uk:443/pride/ws/archive/file/count/assay/"
  count = entryCount(baseURL, assayAccession)

  colnames(count) = c("assayAccession", "fileNbr")
  if (accessionRownames)
    rownames(count) = assayAccession
  return(count)
}

