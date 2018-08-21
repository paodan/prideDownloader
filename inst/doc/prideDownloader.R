## ----setup, eval = FALSE-------------------------------------------------
#  
#  #### run test ####
#  # speciesFilter is the for species, 9606 is homo sapiens.
#  cnt = prideCount(query = "breast", speciesFilter = 9606)
#  cnt
#  srch = prideSearch(query = "breast", speciesFilter = 9606, show = 1000)
#  View(srch)
#  
#  # Quantification
#  srchEx =prideSearchExtra(srch$accession)
#  View(srchEx)
#  
#  lnk = prideLink(rownames(srchEx[srchEx$quantification == "Label free", ]))
#  prideDownload(lnk[1:30, ], fileMaxSize = 40, fileType = "SEARCH",
#                path = "../PXD_Breast/")
#  
#  
#  lnk = prideLink(srch$accession[1:31])
#  View(lnk)
#  prideDownload(lnk[1:50,], fileMaxSize = 4, fileType = "SEARCH",
#                path = "")
#  
#  
#  # available, first 4 files
#  ava = srchEx[srchEx$quantification != "Not available",]
#  lnk = prideLink(rownames(ava))
#  ext3 = substr(lnk$fileName, nchar(lnk$fileName)-2, nchar(lnk$fileName))
#  ext4 = substr(lnk$fileName, nchar(lnk$fileName)-3, nchar(lnk$fileName))
#  table(ext3)
#  table(ext4)
#  ext = tools::file_ext(lnk$fileName)
#  id = ext %in% c(".7z", ".gz", ".zip", ".csv", ".rar", ".tsv", ".txt", ".xls", ".xls")
#  # id = ext3 %in% c(".7z", ".gz") |
#  #   ext4 %in% c(".zip", ".csv", ".rar", ".tsv", ".txt", ".xls", ".xls")
#  # id = ext4 %in% c(".zip")
#  downloadLink = subset(lnk[id, ], fileType == "SEARCH")
#  downloadLink = downloadLink[order(downloadLink$fileSize, decreasing = T), ]
#  write.csv(downloadLink, file = "../sampleLink/downloadLink.csv")
#  
#  prideDownload(prideLinks = downloadLink[downloadLink$fileSize < 10^10, ], fileMaxSize = 60, fileType = "SEARCH",
#                path = "../PXD_Breast/")
#  

