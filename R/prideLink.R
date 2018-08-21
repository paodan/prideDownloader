
################### get links ###################
prideLink = function(ProjectAccessionID){
  result = data.frame()
  t = 1
  for (mi in ProjectAccessionID){
    systim = system.time({
      queryProjList = mi
      query = queryProjList
      # url = paste0('http://www.ebi.ac.uk:80/pride/ws/archive/file/list/project/', query)
      url = paste0('https://www.ebi.ac.uk/pride/ws/archive/file/list/project/', query)
      cat(paste(t,":", mi))
      textJson = fromJSON(getURL(url))
      cat(paste(" done, "))
      t = t+1
      for (ni in 1:length(textJson$list)){
        tmp = textJson$list[[ni]]
        result = rbind(result, data.frame(
          projectAccession = tmp$projectAccession,
          assayAccession = paste0(tmp$assayAccession, collapse = "|"),
          fileType = tmp$fileType,
          fileSource = tmp$fileSource,
          fileSize = tmp$fileSize,
          fileName = tmp$fileName,
          downloadLink = tmp$downloadLink))
      }
    })
    cat(paste("time:", unname(systim[3]), "\n"))
    if (t %% 8 == 0){
      cat("PrideDatabase rule: wait 5 seconds...\n")
      Sys.sleep(5)
    }
    if (t %% 29 == 0){
      cat("PrideDatabase rule: wait 10 seconds...\n")
      Sys.sleep(10)
    }
  }
  return(result)
}
