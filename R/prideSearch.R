
################### search ###################
prideSearch = function(query = "*",#query = "breast",
                       speciesFilter = NULL, #9606 is Homo sapiens
                       ptmsFilter = NULL,
                       tissueFilter = NULL,
                       diseaseFilter = NULL,
                       titleFilter = NULL,
                       instrumentFilter = NULL,
                       experimentTypeFilter = NULL,
                       quantificationFilter = NULL,
                       projectTagFilter = NULL,
                       show = 100,#count # the maximum show is 2000
                       page = 0){
  queryAll = unlist(as.list(environment()))
  count = prideCount(query, speciesFilter, ptmsFilter, tissueFilter,
                     diseaseFilter,titleFilter, instrumentFilter,
                     experimentTypeFilter,quantificationFilter,
                     projectTagFilter)
  result1 = data.frame()
  for (i in 0:(ceiling(count/show) - 1)){
    queryAll["page"] = i
    queryAll = data.frame(type = names(queryAll), unname(queryAll))
    queryAll = paste(sapply(1:nrow(queryAll), function(x){
      paste(queryAll[x,1],queryAll[x,2], sep = "=")}), collapse  = "&")
    # url = paste0('http://www.ebi.ac.uk:80/pride/ws/archive/project/list?', queryAll)
    url = paste0('https://www.ebi.ac.uk/pride/ws/archive/project/list?', queryAll)
    # getting webpages
    text = getURL(url)
    if (length(grep("failure.html", text)) != 0){
      message("********** getURL fail **********")
      message("please try to reduce the number in query: 'show'!")
    } else{
      textJson = fromJSON(text)

      result = data.frame()
      for (mi in 1:length(textJson$list)){
        tmp = textJson$list[[mi]]
        result = rbind(result, data.frame(
          accession = tmp$accession,
          title = tmp$title,
          projectDescription = paste0(tmp$projectDescription, collapse = "|"),
          publicationDate = tmp$publicationDate,
          submissionType = tmp$submissionType,
          numAssays = tmp$numAssays,
          species = paste0(tmp$species, collapse = "|"),
          tissues = paste0(tmp$tissues, collapse = "|"),
          ptmNames = paste0(tmp$ptmNames, collapse = "|"),
          instrumentNames = paste0(tmp$instrumentNames, collapse = "|"),
          projectTags = paste0(tmp$projectTags, collapse = "|")))
      }
      result1 = rbind(result1, result)
    }
  }
  return(result1)
}
