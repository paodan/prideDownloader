#' The assay entries information in the projects
#' @param projectAccession character, a project accession number
#' for example: PXD000001.
#' @return a data.frame of the details of assay entries information, including:
#' projectAccession (string, optional): the project this assay belongs to,
#' assayAccession (string, optional): the accession assigned to the assay,
#' title (string, optional): the title give to the assay by the submitter,
#' shortLabel (string, optional): the short label give to the assay by the submitter,
#' species (array[string], optional): the species reported for this assay,
#' sampleDetails (array[string], optional): sample details reported for this assay
#' ptmNames (array[string], optional): reported modifications,
#' instrumentNames (array[string], optional): reported instrument information,
#' keywords (string, optional): additional keywords added to this assay,
#' experimentalFactor (string, optional): experimental factors reported for this assay,
#' proteinCount (integer, optional): number of proteins in this assay,
#' peptideCount (integer, optional): number of peptides in this assay,
#' uniquePeptideCount (integer, optional): number of unique peptides in this assay,
#' identifiedSpectrumCount (integer, optional): number of identified spectra in this assay,
#' totalSpectrumCount (integer, optional): total number of spectra in this assay,
#' ms2Annotation (boolean, optional): flag if ms level 2 annotation is available,
#' chromatogram (boolean, optional): flag to indicate if a chromatogram is available,
#' softwares (array[string], optional): software used for the data/result generation,
#' diseases (array[string], optional): disease annotation provided for this assay (if applicable),
#' quantMethods (array[string], optional): quantification methods used,
#' contacts (array[ContactDetail], optional): contact persons, usually the submitter of the dataset,
#' @import RCurl
#' @import RJSONIO
#' @import XML
#' @export
#' @examples {
#' \dontrun{
#' # Search for one project accession
#' assayList("PXD000001")
#'
#' # Search for two project accessions
#' assayList(c("PXD000001", "PXD008959"))
#' }
#' }
assayList = function(projectAccession, accessionRownames = TRUE){
  .assayList = function(acc){
    # This URL is provided in PRIDE homepage -> Access data -> Web Service
    url = paste0('https://www.ebi.ac.uk:443/pride/ws/archive/assay/list/project/', acc)
    # https://www.ebi.ac.uk:443/pride/ws/archive/assay/list/project/PXD000001
    cat("The URL is:", url, "\n")
    txt = fromJSON(getURL(url))[[1]]
    if (length(txt) > 0){
      txt = txt[[1]]
    } else {
      txt = setNames(rep("", 21), .assayDetailName)
      txt[1] = acc
    }
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

  len = length(projectAccession)
  if (len < 1){
    stop("Must provide at least one projectAccession number!")
  } else if (len == 1){
    res = as.data.frame(t(.assayList(projectAccession)),
                        stringsAsFactors = FALSE)
  } else {
    res = matrix("", nrow = len, ncol = 21)
    t = 0
    for (mi in projectAccession){
      t = t + 1
      i = .assayList(mi)
      res[t,] = i
      if ((t %% 20 == 0) && t != len) Sys.sleep(15)
    }
    colnames(res) = .assayDetailName
  }
  if (accessionRownames)
    rownames(res) = projectAccession
  return(res)
}
