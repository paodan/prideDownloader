#' The number of assays in the projects
#' @param projectAccession character, a project accession number,
#' for example: PXD000001.
#' @return The number of assays in the projects.
#' @import RCurl
#' @import RJSONIO
#' @import XML
#' @export
#' @examples {
#' \dontrun{
#' # Search for one project accession
#' assayCount("PXD000001")
#'
#' # Search for two project accessions
#' assayCount(c("PXD000001", "PXD008959"))
#' }
#' }
assayCount = function(projectAccession = c("PXD000001", "PXD008959"),
                      accessionRownames = TRUE){
  .assayCount = function(acc){
    # This URL is provided in PRIDE homepage -> Access data -> Web Service
    url = paste0('https://www.ebi.ac.uk:443/pride/ws/archive/assay/count/project/', acc)
    # https://www.ebi.ac.uk:443/pride/ws/archive/assay/count/project/PXD000001
    cat("The URL is:", url, "\n")
    count = as.numeric(getURL(url))
    return(count)
  }
  res = data.frame(projectAccession = projectAccession,
                   assayNbr = unlist(lapply(projectAccession, .assayCount)))
  if (accessionRownames)
    rownames(res) = projectAccession
  return(res)
}
