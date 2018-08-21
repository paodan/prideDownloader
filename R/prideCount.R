#' The number of entries returned by searching
#' @param query character, the keywords to search. For example: "stress".
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
#' @return the number of entries
#' @examples {
#' \dontrun{
#' # search "breast" on Homo sapiens (9606)
#' prideCount(query = "breast", speciesFilter = 9606)
#' }
#' }
################### count ###################
prideCount = function(query = "*",#query = "breast",
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
  cat("\n")
  queryAll = data.frame(type = names(queryAll), unname(queryAll))
  print(queryAll)
  queryAll = paste(sapply(1:nrow(queryAll), function(x){
    paste(queryAll[x,1],queryAll[x,2], sep = "=")}), collapse  = "&")
  print(queryAll)
  # This URL is provided in PRIDE homepage -> Access data -> Web Service
  url = paste0('https://www.ebi.ac.uk/pride/ws/archive/project/count?', queryAll)
  # http://www.ebi.ac.uk/pride/ws/archive/project/count?query=breast&speciesFilter=9606
  print(url)
  count = as.numeric(getURL(url))
  cat("count = ", count, "\n")
  return(count)
}
