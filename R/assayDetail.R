#' The details of assays returned by searching
#' @param accession character, a vector of assay accession. The default is
#' c("22134", "22135")
#' @return a data.frame of assay details, including:
#' projectAccession, assayAccession, title, shortLabel, species,
#' sampleDetails, ptmNames, instrumentNames, keywords, experimentalFactor,
#' proteinCount, peptideCount, uniquePeptideCount, identifiedSpectrumCount,
#' totalSpectrumCount, ms2Annotation, chromatogram, softwares, diseases,
#' quantMethods, and contacts.
#' @import RCurl
#' @import RJSONIO
#' @import XML
#' @export
#' @examples {
#' \dontrun{
#' # Search for one accession
#' assayDetail("22134")
#'
#' # Search for two accessions
#' assayDetail(c("22134", "22135"))
#' }
#' }
assayDetail = function(accession = c("22134", "22135"), accessionRownames = TRUE){
  .assayDetail = function(acc){
    # This URL is provided in PRIDE homepage -> Access data -> Web Service
    url = paste0('https://www.ebi.ac.uk:443/pride/ws/archive/assay/', acc)
    # http://www.ebi.ac.uk/pride/ws/archive/project/count?query=breast&speciesFilter=9606
    cat("The URL is:", url, "\n")
    txt = fromJSON(getURL(url))
    f = function(x, sep = "|"){
      y = if (is.null(unlist(x))) { ""
      } else paste0(unlist(x), collapse = sep)
      y
    }
    detail = unlist(lapply(txt, f))
    if ("status" %in% names(detail)){
      detail = paste0(acc, rep(": private data!", 21))
      names(detail) = .assayDetailName
    }
    return(detail)
  }

  len = length(accession)
  if (len < 1){
    stop("Must provide at least one accession number!")
  } else if (len == 1){
    res = as.data.frame(t(.assayDetail(accession)),
                        stringsAsFactors = FALSE)
  } else {
    res = matrix("", nrow = len, ncol = 21)
    t = 0
    for (mi in accession){
      t = t + 1
      i = .assayDetail(mi)
      res[t,] = i
      if ((t %% 20 == 0) && t != len) Sys.sleep(15)
    }
    colnames(res) = .assayDetailName
  }
  if (accessionRownames)
    rownames(res) = accession
  return(res)
}
