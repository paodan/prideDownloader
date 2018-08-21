#' The details of projects returned by searching
#' @param accession character, a vector of project accessions. The default is
#' c("PXD000001", "PXD000002")
#' @return a data.frame of project details, including: projectAccession, title,
#' numAssays, species, tissues, ptmNames, quantificationMethods, numProteins,numPeptides,
#' numSpectra, numIdentifiedSpectra, numUniquePeptides, instrumentNames, experimentTypes,
#' keywords, publicationDate, projectDescription, submissionType, projectTags, submitter,
#' submissionDate, reanalysis, sampleProcessingProtocol, dataProcessingProtocol,
#' otherOmicsLink, labHeads, doi, and references.
#' @import RCurl
#' @import RJSONIO
#' @import XML
#' @export
#' @examples {
#' \dontrun{
#' # Search for one accession
#' projectDetail("PXD000002")
#'
#' # Search for two accessions
#' projectDetail(c("PXD000001", "PXD000002"))
#' }
#' }
################### count ###################
projectDetail = function(accession = c("PXD000001", "PXD000002"),
                         accessionRownames = TRUE){
  .projectDetail = function(acc){
    # This URL is provided in PRIDE homepage -> Access data -> Web Service
    url = paste0('https://www.ebi.ac.uk:443/pride/ws/archive/project/', acc)
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
      detail = paste0(acc, rep(": private data!", 28))
      names(detail) = .projectDetailName
    }
    detail = detail[.projectDetailName]
    names(detail)[1] = "projectAccession"
    # detail = c(
    #   projectAccession = f(txt$accession), title = f(txt$title),
    #   numAssays = f(txt$numAssays), species = f(txt$species),
    #   tissues = f(txt$tissue), ptmNames = f(txt$ptmNames),
    #   quantificationMethods = f(txt$quantificationMethods),
    #   numProteins = f(txt$numProteins), numPeptides = f(txt$numPeptides),
    #   numSpectra = f(txt$numSpectra),
    #   numIdentifiedSpectra = f(txt$numIdentifiedSpectra),
    #   numUniquePeptides = f(txt$numUniquePeptides),
    #   instrumentNames = f(txt$instrumentNames),
    #   experimentTypes = f(txt$experimentTypes),
    #   keywords = f(txt$keywords), publicationDate = f(txt$publicationDate),
    #   projectDescription = f(txt$projectDescription),
    #   submissionType = f(txt$submissionType),
    #   projectTags = f(txt$projectTags), submitter = f(txt$submitter, " "),
    #   submissionDate = f(txt$submissionDate), reanalysis = f(txt$reanalysis),
    #   sampleProcessingProtocol = f(txt$sampleProcessingProtocol),
    #   dataProcessingProtocol = f(txt$dataProcessingProtocol),
    #   otherOmicsLink = f(txt$otherOmicsLink),
    #   labHeads = f(txt$labHeads), doi = f(txt$doi),
    #   references = f(txt$references))
    return(detail)
  }

  len = length(accession)
  if (len < 1){
    stop("Must provide at least one accession number!")
  } else if (len == 1){
    res = as.data.frame(t(.projectDetail(accession)),
                        stringsAsFactors = FALSE)
  } else {
    res = matrix("", nrow = len, ncol = 28)
    t = 0
    for (mi in accession){
      t = t + 1
      # i = as.character(.projectDetail(mi))
      i = .projectDetail(mi)
      res[t,] = i
      if ((t %% 20 == 0) && t != len) Sys.sleep(15)
    }
    colnames(res) = names(i)
  }

  if (accessionRownames)
    rownames(res) = accession
  return(res)
}
