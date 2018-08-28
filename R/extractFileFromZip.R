#' Extract files from zip file.
#' @param zipFileName the name of the zip file.
#' @param zipPath the path of the zip file.
#' @param fileNamePattern the files with the this name pattern will be extracted.
#' @param ignore.case logic, ignore the letter case of fileNamePattern.
#' @param savePath the path to save the extracted files. The default is the current
#' directory.
#' @param filePrefix character, the prefix name to add in front of the file names.
#' The default is NULL, which will automatically extract the first part of zipFileName
#' seperated by "_".
#' @return a status of extraction, and extracted files in the savePath.
#' @import tools
#' @import limma
#' @examples {
#' \dontrun{
#' extractFileFromZip(c("file1.zip", "file2.zip"), zipPath = "./",
#'                    fileNamePattern = "proteinGroup",
#'                    ignore.case = TRUE,
#'                    savePath = "./",
#'                    filePrefix = NULL)
#' }
#' }
extractFileFromZip = function(zipFileName ="PXD008222_search_dummy_txt.zip",
                              zipPath = "./",
                              fileNamePattern = "proteinGroup",
                              ignore.case = TRUE,
                              savePath = "./",
                              filePrefix = NULL){
  .extractFileFromZip = function(zipFileName,
                                 zipPath,
                                 fileNamePattern,
                                 ignore.case,
                                 savePath,
                                 filePrefix){
    # zipFileName = "PXD007620_Trametinib-Sch772984-Dmso_MaxQuant_search_results_output.zip"
    fullName = paste0(zipPath, "/", basename(zipFileName))
    cmd = paste0('unzip -l "', zipFileName, '"')

    fileListInZip = system(cmd, intern = T)
    resFileInfo = grep(fileNamePattern, fileListInZip, value = T, ignore.case = ignore.case)

    # resFileName = limma::strsplit2(resFileInfo, " ")
    resFileName = limma::strsplit2(resFileInfo, "   ")
    resFileName = resFileName[, ncol(resFileName)]

    if (is.null(filePrefix)){
      # filePrefix = paste0(limma::strsplit2(basename(zipFileName), "_")[1,1], "_")
      filePrefix = paste0(tools::file_path_sans_ext(basename(zipFileName)), "_")
    }

    extractCMD = paste0('unzip -j "', fullName, '" "',
                        resFileName, '" -d "', savePath, '/',
                        filePrefix,
                        basename(resFileName), '" -o')
    status = c()
    if (length(extractCMD) > 0){
      for(mi in extractCMD){
        tmp = system(mi, intern = TRUE, input = "o", ignore.stderr = TRUE)
        s = attributes(tmp[1])$status
        status = c(status, (if(is.null(s)) 0 else s))
      }
      names(status) =  basename(resFileName)
    } else {
    }
    return(status)
  }

  res = lapply(zipFileName, .extractFileFromZip, zipPath,
               fileNamePattern, ignore.case, savePath, filePrefix)
  names(res) = basename(zipFileName)
  return(res)
}
