#' The project entries information returned by searching
#' @param query character, the keywords to search. For example: "stress".
#' @param show integer, how many results to return per page. Maximum
#' page size is: 10000 (default).
#' @param page character, which page (starting from 0) of the result to
#' return. The default is 0.
#' @param sort character, the field to sort on (e.g. score, publication_date,
#' id or project_title).
#' @param order character, the sorting order ("asc" or "desc"). The default
#' is "asc"
#' @param speciesFilter numeric, filter by species. The NCBI taxon ID
#' (https://www.ensembl.org/info/about/species.html). For example,
#' 9606 represents Homo sapiens (human), 10116 represents Rattus norvegicus
#' (rat), 9986 represents Oryctolagus cuniculus (rabbit), 10090 is Mus
#' musculus (mouse), and 7955 is Danio rerio (Zebrafish).
#' @param ptmsFilter character, filter by PTM (Post-translational modification)
#' annotation. For example: "phosphorylation".
#' @param tissueFilter character, filter by tissue annotation. For example:
#' "HEK-293 cell", "breast", "cell culture", "brain".
#' @param diseaseFilter character, the disease to filter. For example:
#' "disease free", "cancer".
#' @param titleFilter character, filter the title for keywords (example: stress).
#' @param instrumentFilter character, the instrument to filter. For example:
#' "LTQ Orbitrap", "LTQ", "LTQ FT", "LTQ Orbitrap Velos".
#' @param experimentTypeFilter character, filter by experiment type. For
#' example: "Bottom-up proteomics", "Affinity purification coupled with mass
#' spectrometry proteomics", "Shotgun proteomics", "shotgun".
#' @param quantificationFilter character, the quantification to filter. For
#' example: "Spectrum count/molecular weight", "Spectrum counting", "label-free".
#' @param projectTagFilter character, filter by project tags (example: Biomedical).
#'
#' @return a data.frame of the details of searched entries, including:
#' projectAccession, title, projectDescription, publicationDate, submissionType,
#' numAssays, species tissues, ptmNames, instrumentNames, and projectTags.
#' @import RCurl
#' @import RJSONIO
#' @import XML
#' @export
#' @examples {
#' \dontrun{
#' # Search for stress
#' projectList(query = "stress")
#'
#' # Search for stress and filtered by human
#' projectList(query = "stress", speciesFilter = 9606)
#'
#' # Search for stress and filtered by human and sorted by publication_date
#' projectList(query = "stress", speciesFilter = 9606,
#'             sort = "publication_date", order = "desc")
#' }
#' }
projectList = function(query = "*",#query = "breast",
                       show = 10000, # maximum 10000
                       page = 0, # starting from 0
                       sort = NULL,
                       order = "asc",
                       speciesFilter = NULL, #9606 is Homo sapiens
                       ptmsFilter = NULL,
                       tissueFilter = NULL,
                       diseaseFilter = NULL,
                       titleFilter = NULL,
                       instrumentFilter = NULL,
                       experimentTypeFilter = NULL,
                       quantificationFilter = NULL,
                       projectTagFilter = NULL){
  queryAll = unlist(as.list(environment()))
  cat("Your search query is: \n")
  print(queryAll)
  queryAll = data.frame(type = names(queryAll), unname(queryAll))
  queryAll = paste(sapply(1:nrow(queryAll), function(x){
    paste(queryAll[x,1],queryAll[x,2], sep = "=")}), collapse  = "&")
  # This URL is provided in PRIDE homepage -> Access data -> Web Service
  url = paste0('https://www.ebi.ac.uk:443/pride/ws/archive/project/list?', queryAll)
  # https://www.ebi.ac.uk:443/pride/ws/archive/project/list?query=stress&show=10000&page=0&order=desc
  cat("The URL is:", url, "\n")
  txt = fromJSON(getURL(url))[[1]]
  f = function(x, sep = "|"){
    y = if (is.null(unlist(x))) { ""
    } else paste0(unlist(x), collapse = sep)
    y
  }

  res = vapply(txt, FUN = function(x){
    # detail = c(
    #   projectAccession = f(x$accession),
    #   title = f(x$title),
    #   projectDescription = f(x$projectDescription),
    #   publicationDate = f(x$publicationDate),
    #   submissionType = f(x$submissionType),
    #   numAssays = f(x$numAssays),
    #   species = f(x$species),
    #   tissues = f(x$tissue),
    #   ptmNames = f(x$ptmNames),
    #   instrumentNames = f(x$instrumentNames),
    #   projectTags = f(x$projectTags)
    # )
    detail = unlist(lapply(x, f))
    detail
  }, FUN.VALUE = rep("", 11))

  res = as.data.frame(t(res))
  colnames(res)[1] = "projectAccession"
  rownames(res) = res$projectAccession
  return(res)
}
