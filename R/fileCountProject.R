#' The number of files in the projects
#' @param projectAccession character, a vector of project accessions.
#' The default is c("PXD000001", "PXD000002")
#' @return a data.frame, the number of files in the projects.
#' @import RCurl
#' @import RJSONIO
#' @export
#' @examples {
#' \dontrun{
#' fileCountProject("PXD000002")
#' fileCountProject(c("PXD000001", "PXD000002"))
#' }
#' }
fileCountProject = function(projectAccession = c("PXD000001", "PXD000002"),
                            accessionRownames = TRUE){

  if (length(projectAccession) < 1){
    stop("Must provide at least one projectAccession!")
  }

  baseURL = "https://www.ebi.ac.uk:443/pride/ws/archive/file/count/project/"
  count = entryCount(baseURL, projectAccession)

  colnames(count) = c("projectAccession", "fileNbr")
  if (accessionRownames)
    rownames(count) = projectAccession
  return(count)
}

