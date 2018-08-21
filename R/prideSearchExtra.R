
################### search for extra information of project ###################
prideSearchExtra = function(ProjectAccessionIDs){

  .searchExtra = function(ProjectAccessionID = "PXD008347"){
    url = paste0("https://www.ebi.ac.uk/pride/archive/projects/", ProjectAccessionID)
    html = getURL(url)
    doc = htmlParse(html, asText = TRUE)

    pathBase = "/html/body/div[2]/div/section/div/div[3]/div[2]/"
    apendix = c(Species = "div[1]/div[1]/p/a",
                Instrument = "div[3]/div[1]/p/a",
                Modification = "div[4]/div[1]/p/a",
                ExperimentType = "div[5]/div[1]/p/a",
                Tissue = "div[1]/div[2]/p/a",
                Software = "div[3]/div[2]/p/a",
                quantification = "div[4]/div[2]/p/a")

    xpath = paste0(pathBase, apendix)
    res = c()
    for(ni in seq_along(apendix)){
      tmp = paste0(xpathSApply(doc = doc, xpath[ni], xmlValue), collapse = "|")
      tmp = gsub("[\n][^A-Za-z]+", "", tmp)
      res = c(res, tmp)
    }
    res[res == ""] = "Not available"
    names(res) = names(apendix)
    return(res)
  }

  n = length(ProjectAccessionIDs)
  srchEx = matrix("", nrow = n, ncol = 7,
                  dimnames = list(ProjectAccessionIDs,
                                  names(.searchExtra(ProjectAccessionIDs[1]))))
  for(mi in seq_along(ProjectAccessionIDs)){
    m = ProjectAccessionIDs[mi]
    srchEx[m,] = .searchExtra(m)
    cat(mi, ":", m, "done\n")
    # Sys.sleep(1)
    if (((mi %% 10) == 0) && (mi != n)) {
      Sys.sleep(10)
      cat("PrideDatabase rule: wait 10 seconds")
    }
  }
  return(as.data.frame(srchEx))
}
