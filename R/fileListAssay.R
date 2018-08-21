#' List files for a project
#' @param assayAccession character, a vector of project accessions.
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
#' fileListAssay("22134")
#'
#' # Search for two accessions
#' fileListAssay(c("22134", "22135"))
#' }
#' }
fileListAssay = function(assayAccession = c("22134", "22135")){

  if (length(assayAccession) < 1){
    stop("Must provide at least one assayAccession!")
  }

  urlBase = "https://www.ebi.ac.uk:443/pride/ws/archive/file/list/assay/"
  res = entryCount(baseURL, assayAccession)

  return(res)
}
