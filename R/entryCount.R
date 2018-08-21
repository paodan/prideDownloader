#' @import RCurl
#' @import RJSONIO
#' @importFrom tools file_ext
entryCount = function(urlBase, urlRest){
# Basic function for fileCountProject and fileCountAssay functions
  .entryCount = function(url){
    # This URL is provided in PRIDE homepage -> Access data -> Web Service
    # url = paste0('https://www.ebi.ac.uk:443/pride/ws/archive/file/count/project/', acc)
    # https://www.ebi.ac.uk:443/pride/ws/archive/file/count/project/PXD000001
    cat("The URL is:", url, "\n")
    Sys.sleep(0.5)
    count = suppressWarnings(as.numeric(getURL(url)))
    return(count)
  }

  urls = paste0(urlBase, urlRest)
  count = vapply(urls, .entryCount, 1)

  res = data.frame(key = urlRest,
                   count = count,
                   stringsAsFactors = FALSE)
  res$count[is.na(res$count)] = 0
  rownames(res) = 1:nrow(res)
  return(res)
}
