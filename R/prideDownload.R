
################ download ################
# slow and depends on Internet connection
prideDownload = function(prideLinks, fileMaxSize = 10, fileType = "SEARCH", path = getwd()){
  # prideLinks is the result from prideLink()
  link = prideLinks[(prideLinks$fileType %in% fileType),"downloadLink"]
  # what is the size (Gb) of these files
  totalSize = sum(prideLinks[(prideLinks$fileType %in% fileType), "fileSize"])/1000^3
  if (totalSize > fileMaxSize){
    stop("The size of total files is ",totalSize, " Gb > ", fileMaxSize,
         " Gb! \nPlease change fileMaxSize or reduce number of links")
  }
  cat("Total size of file is", totalSize,"Gb\n")
  if (substring(path, first = nchar(path)) != "/")
    path = paste0(path, "/")
  len = length(unique(link))
  t = 1
  for (ki in unique(link)){
    cat(t, "/",len,": Downloading", ki,"\n")
    t = t+1
    # tmp = strsplit(ki,"/")[[1]]
    # cmd = paste0('wget -q -O "', path,
    #              tmp[length(tmp)-1], "_", tmp[length(tmp)], '" "', ki, '"')
    # system(cmd)
    fileKi = paste0(path, basename(dirname(ki)), "_", basename(ki))
    download.file(ki, fileKi)
    if (t %% 10 == 0) Sys.sleep(abs(rnorm(1, mean = 10, sd = 5)))
  }
}
