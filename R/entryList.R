
#' @import RCurl
#' @import RJSONIO
#' @importFrom tools file_ext

entryList = function(urlBase, urlRest){
# Basic function for fileListProject and fileListAssay functions
  .entryList = function(url){
    # This URL is provided in PRIDE homepage -> Access data -> Web Service
    # url = paste0('https://www.ebi.ac.uk:443/pride/ws/archive/file/count/project/', acc)
    # https://www.ebi.ac.uk:443/pride/ws/archive/file/count/project/PXD000001
    cat("The URL is:", url, "\n")
    Sys.sleep(0.5)
    txt = fromJSON(getURL(url))
    f = function(x, sep = "|"){
      y = if (is.null(unlist(x))) { ""
      } else paste0(unlist(x), collapse = sep)
      y
    }

    col = length(.fileListProName)
    if (length(txt) == 6){
      print("private")
      # Private data, or not found.
      detail = data.frame(t(paste0(acc, rep(": private data!", col))),
                          stringsAsFactors = FALSE)
      names(detail) = .fileListProName
      detail$fileSize = "0"
    } else {
      detail = vapply(txt[[1]], function(x){
        y = unlist(lapply(x, f))
      }, rep("", col))
      detail = data.frame(t(detail), stringsAsFactors = FALSE)
    }
    detail
  }

  urls = paste0(urlBase, urlRest)
  res = .entryList(urls[1])
  if(length(urls) > 1){
    for(mi in 2:length(urls)){
      res = rbind(res, .entryList(urls[mi]))
    }
  }

  # file extension
  res$fileExtension = tools::file_ext(res$fileName)
  # file size in GB
  res$fileSizeGB = as.numeric(res$fileSize)/10^9
  return(res)
}
