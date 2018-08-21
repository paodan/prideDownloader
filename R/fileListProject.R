#' List files for a project
#' @param projectAccession character, a vector of project accessions.
#' The default is c("PXD000001", "PXD000002")
#' @return a data.frame of the details of searched entries, including:
#' projectAccession (string, optional): ,
#' assayAccession (string, optional): ,
#' fileType (string, optional) = ['RESULT' or 'PEAK' or 'SEARCH' or 'RAW' or 'QUANT' or 'GEL' or 'FASTA' or 'SPECTRUM_LIBRARY' or 'MS_IMAGE_DATA' or 'OPTICAL_IMAGE' or 'OTHER']: ,
#' fileSource (string, optional): SUBMITTED (part of the original dataset) or GENERATED (added to the submission by PRIDE),
#' fileSize (integer, optional): size in bytes,
#' fileName (string, optional): the name of the file,
#' downloadLink (string, optional): public FTP download link,
#' asperaDownloadLink (string, optional): public Aspera download link,
#' fileExtension: file extension,
#' fileSizeGB: file size in GB.
#' @import RCurl
#' @import RJSONIO
#' @importFrom tools file_ext
#' @export
#' @examples {
#' \dontrun{
#' # Search for one accession
#' fileListProject("PXD000002")
#'
#' # Search for two accessions
#' fileListProject(c("PXD000001", "PXD000002"))
#' }
#' }
fileListProject = function(projectAccession = c("PXD000001", "PXD000002")){

  if (length(projectAccession) < 1){
    stop("Must provide at least one projectAccession!")
  }

  urlBase = "https://www.ebi.ac.uk:443/pride/ws/archive/file/list/project/"
  res = entryList(urlBase, projectAccession)

  return(res)
}


# fileListProject = function(projectAccession = c("PXD000001", "PXD000002")){
#
#   if (length(projectAccession) < 1){
#     stop("Must provide at least one projectAccession!")
#   }
#
#   .fileListProject = function(acc){
#     # This URL is provided in PRIDE homepage -> Access data -> Web Service
#     url = paste0('https://www.ebi.ac.uk:443/pride/ws/archive/file/list/project/', acc)
#     # http://www.ebi.ac.uk/pride/ws/archive/project/count?query=breast&speciesFilter=9606
#     cat("The URL is:", url, "\n")
#     txt = fromJSON(getURL(url))
#     f = function(x, sep = "|"){
#       y = if (is.null(unlist(x))) { ""
#       } else paste0(unlist(x), collapse = sep)
#       y
#     }
#
#     col = length(.fileListProName)
#     if (length(txt) == 6){
#       print("private")
#       # Private data, or not found.
#       detail = data.frame(t(paste0(acc, rep(": private data!", col))),
#                           stringsAsFactors = FALSE)
#       names(detail) = .fileListProName
#       detail$fileSize = "0"
#     } else {
#       detail = vapply(txt[[1]], function(x){
#         y = unlist(lapply(x, f))
#         y[.fileListProName]
#       }, rep("", col))
#       detail = data.frame(t(detail), stringsAsFactors = FALSE)
#     }
#     detail
#   }
#
#   resSub = lapply(setNames(projectAccession, projectAccession),
#                   FUN = .fileListProject)
#   # combine
#   res = do.call(rbind, resSub)
#   # file extension
#   res$fileExtension = tools::file_ext(res$fileName)
#   # file size in GB
#   res$fileSizeGB = as.numeric(res$fileSize)/10^9
#   return(res)
# }
