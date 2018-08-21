#' The details of projects returned by searching
#' @param prideLinks data.frame, the result from \code{\link{fileListProject}}
#' or \code{\link{fileListAssay}} function.
#' @param fileMaxSize numeric, maximumal total file size.
#' @param fileType character, "RESULT", "PEAK", "SEARCH", "RAW", "OTHER".
#' The default is "SEARCH".
#' @param fileExtensionInclude character, the files with these extensions
#' are included to be downloaded. For example: "csv", "xlsx", "xls", "7z", etc.
#' @param fileExtensionExclude character, the files with these extensions
#' are excluded. For example: "raw", "fasta", "gz", "xml", etc.
#' @param path character, the path to save downloaded files.
#' @return data.frame, information of downloaded files.
#' @import RCurl
#' @import RJSONIO
#' @export
#' @examples {
#' \dontrun{
#' # Search for one accession
#' fileInfo = fileListProject(c("PXD000001", "PXD000002"))
#' prideDownload(prideLinks = fileInfo,
#'               fileMaxSize = 10,
#                fileType = "SEARCH",
#                fileExtensionInclude = NULL,
#                fileExtensionExclude = "raw",
#                path = getwd())
#' }
#' }
prideDownload = function(prideLinks,
                         fileMaxSize = 10,
                         fileType = "SEARCH",
                         fileExtensionInclude = NULL,
                         fileExtensionExclude = NULL,
                         path = getwd()){
  # prideLinks is the result from fileListProject or fileListAssay function
  cat("fileType:\n")
  print(table(prideLinks$fileType))
  cat("fileExtension:\n")
  print(table(prideLinks$fileExtension))
  id1 = prideLinks$fileType %in% fileType
  id2 = id3 = id1
  if (!is.null(fileExtensionInclude)){
    id2 = prideLinks$fileExtension %in% fileExtensionInclude
  }
  if (!is.null(fileExtensionExclude)){
    id3 = !prideLinks$fileExtension %in% fileExtensionExclude
  }
  id = id1 & id2 & id3
  link = prideLinks[id, "downloadLink"]
  # what is the size (Gb) of these files
  totalSize = sum(as.numeric(prideLinks[id, "fileSize"]))/1000^3
  if (totalSize > fileMaxSize){
    stop("The size of total files is ",totalSize, " Gb > ", fileMaxSize,
         " Gb! \nPlease change fileMaxSize or reduce number of links")
  }
  cat("Total size of file is", totalSize,"Gb\n")
  if (substring(path, first = nchar(path)) != "/")
    path = paste0(path, "/")
  len = length(unique(link))
  t = 1
  fileDown = c()
  for (ki in unique(link)){
    cat(t, "/",len,": Downloading", ki,"\n")
    t = t+1
    fileKi = paste0(path, basename(dirname(ki)), "_", basename(ki))
    download.file(ki, fileKi)
    fileDown = c(fileDown, fileKi)
    if (t %% 10 == 0) Sys.sleep(abs(rnorm(1, mean = 8, sd = 3)))
  }
  fileDownload = prideLinks[id,]
  fileDownload$path = fileDown
  return(fileDownload)
}
